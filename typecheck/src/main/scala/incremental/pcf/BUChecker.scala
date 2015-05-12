package incremental.pcf

import constraints.Statistics
import constraints.equality._
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
    val subcs = e.foldKids(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._3)
    val cs = subcs addNewConstraints cons
    val reqs2 = cs.applyPartialSolutionIt[(Symbol, T), Map[Symbol, T], T](reqs, p => p._2)
    e.typ = (cs applyPartialSolution t, reqs2, cs.propagate)
  }

  def typecheckStep(e: Node_[Result]): StepResult = e match {
    case Num(_) => (TNum, Map(), Seq())
    case Add(e1, e2) =>
      val (t1, reqs1, _) = e1.typ
      val (t2, reqs2, _) = e2.typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (TNum, mreqs, mcons :+ lcons :+ rcons)
    case Mul(e1, e2) =>
      val (t1, reqs1, _) = e1.typ
      val (t2, reqs2, _) = e2.typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (TNum, mreqs, mcons :+ lcons :+ rcons)
    case Var(x) =>
      val X = freshUVar()
      (X, Map(x -> X), Seq())
    case App(e1,e2) =>
      val (t1, reqs1, _) = e1.typ
      val (t2, reqs2, _) = e2.typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (X, mreqs, mcons :+ fcons)
    case Abs(x, tx, e) =>
      val (t, reqs, _) = e.typ

      reqs.get(x) match {
        case None =>
          val X = tx.getOrElse(freshUVar())
          (TFun(X, t), reqs, Seq())
        case Some(treq) =>
          val otherReqs = reqs - x
          if (tx.isDefined) {
            (TFun(treq, t), otherReqs, Seq(EqConstraint(tx.get, treq)))
          }
          else
            (TFun(treq, t), otherReqs, Seq())
      }
    case AbsMany(xs, e) =>
      val (t, reqs, _) = e.typ
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
    case If0(e1, e2, e3) =>
      val (t1, reqs1, _) = e1.typ
      val (t2, reqs2, _) = e2.typ
      val (t3, reqs3, _) = e3.typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)
      
      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      (t2, mreqs, mcons :+ cond :+ body)

    case Fix(e) =>
      val (t, reqs, _) = e.typ
      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      (X, reqs, Seq(fixCons))
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
          mcons = EqConstraint(r1, r2) +: mcons
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