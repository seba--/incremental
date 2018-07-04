package incremental.pcf.let_poly

import constraints.Statistics
import constraints.equality_letpoly._
import incremental.{Node_, Util}
import incremental.Node.Node

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {
  import csFactory._

  type TError = String
  type Reqs = Map[Symbol, Type]

  type StepResult = (Type, Reqs, Seq[Constraint])
  type Result = (Type, Reqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        typecheckRec(e)
        true
      }

      val (t_, reqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)
      println(sol_)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
  }

  def typecheckRec(e: Node_[Result]): Unit = {
    val res@(t, reqs, cons) = typecheckStep(e)
    val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._3)
    val cs = subcs addNewConstraints cons
    val reqs2 = cs.applyPartialSolutionIt[(Symbol, T), Map[Symbol, T], T](reqs, p => p._2)
    e.typ = (cs applyPartialSolution t, reqs2, cs.propagate)
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case Num => (TNum, Map(), Seq())
    case Float => (TFloat, Map(), Seq())
    case Char => (TChar, Map(), Seq())
//    case Bool => (TBool, Map(), Seq())
//    case isLower =>
//      val (t, reqs, _) = e.kids(0).typ
//      val cons = EqConstraint(t, TChar)
//
//      (TBool, reqs, Seq(cons))

    case op if op == Add || op == Mul =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (TNum, mreqs, mcons :+ lcons :+ rcons)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> X), Seq())
    case VarL =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val u = freshUVar()
      val X = freshUSchema()
      val cons = InstConstraint(u, X)
      (u, Map(x -> X), Seq(cons))
    case App =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (X, mreqs, mcons :+ fcons)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, _) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
          (TFun(X, t), reqs, Seq())
        case Some(treq) =>
          val otherReqs = reqs - x
          val X = freshUVar()
          if (e.lits.size == 2) {
            (TFun(X, t), otherReqs, Seq(EqConstraint(treq, X), EqConstraint(e.lits(1).asInstanceOf[Type], treq)))
          }
          else
            (TFun(X, t), otherReqs, Seq(EqConstraint(treq, X)))
      }
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, _) = e.kids(0).typ

      val Xs = xs map (_ => freshUVar())

      var restReqs = reqs
      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val x = xs(i)
        restReqs.get(x) match {
          case None =>
            val X = freshUVar()
            tfun = TFun(X, tfun)
          case Some(treq) =>
            restReqs = restReqs - x
            tfun = TFun(treq, tfun)
        }
      }

      (tfun, restReqs, Seq())
    case If0 =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ
      val (t3, reqs3, _) = e.kids(2).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)
      
      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      (t2, mreqs, mcons :+ cond :+ body)

    case Fix =>
      val (t, reqs, _) = e.kids(0).typ
      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      (X, reqs, Seq(fixCons))

    case LetV =>
      val (t2, reqs2, _) = e.kids(1).typ

      val (t1, reqs1, _) = e.kids(0).typ
      val X = e.lits(0).asInstanceOf[Symbol]

      val (mcons, mreq) = mergeReqMaps(reqs1, reqs2)
      var cons = mcons

      var resreq = mreq

      reqs2.get(X) match {
        case None => cons
        case Some(typ) => resreq = resreq - X
          cons = cons :+ GenConstraint(typ, t1, reqs1, reqs2)
      }
      (t2, resreq, cons )
  }


  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    }

  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          (r1, r2) match {
            case (_, InstS(_)) => mcons = EqConstraint(r1, r2) +: mcons
            case (InstS(_), _) => mcons = EqConstraint(r1, r2) +: mcons
            case (InstS(_), InstS(_)) => mcons
              mreqs += x -> r2
            case (_ , _) => mcons = EqConstraint(r1, r2) +: mcons
          }
      }
    (mcons, mreqs)
  }

}



case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}