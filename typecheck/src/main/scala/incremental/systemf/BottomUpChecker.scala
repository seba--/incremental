package incremental.systemf

import constraints.equality.{EqConstraint, Constraint, Type, ConstraintSystem}
import incremental.Node._
import incremental.{Node_, Util}

/**
 * Created by seba on 13/11/14.
 */
class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = Type.Companion.TError

  type Reqs = Map[Symbol, Type]
  type TReqs = Set[Symbol]

  type StepResult = (Type, Reqs, TReqs, Seq[Constraint])
  type Result = (Type, Reqs, TReqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    val (res, ctime) = Util.timed {
      root.visitUninitialized {e =>
        val (t, reqs, treqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
        val cs = subcs addNewConstraints cons
        val reqs2 = cs.applyPartialSolutionIt[(Symbol,Type),Map[Symbol,Type]](reqs, p => p._2)
        e.typ = (cs applyPartialSolution t, reqs2, treqs, cs.propagate)
        true
      }

      val (t_, reqs, treqs, cs_) = root.typ
      val cs = cs_.tryFinalize
      val t = t_.subst(cs.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${cs.unsolved}")
      else if (!cs.isSolved)
        Right(s"Unresolved constraints ${cs.unsolved}, type $t")
      else
        Left(t)
    }
    localState.stats.typecheckTime += ctime
    res
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case Num => (TNum, Map(), Set(), Seq())
    case op if op == Add || op == Mul =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      (TNum, mreqs, treqs1 ++ treqs2, mcons :+ lcons :+ rcons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> X), Set(), Seq())

    case App =>

      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (X, mreqs, treqs1 ++ treqs2, mcons :+ fcons)

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs, _) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
          (TFun(X, t), reqs, treqs ++ X.freeTVars, Seq())
        case Some(treq) =>
          val otherReqs = reqs - x
          if (e.lits.size == 2) {
            val X = e.lits(1).asInstanceOf[Type]
            (TFun(treq, t), otherReqs, treqs ++ X.freeTVars, Seq(EqConstraint(X, treq)))
          }
          else
            (TFun(treq, t), otherReqs, treqs, Seq())
      }
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, treqs, _) = e.kids(0).typ

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

      (tfun, restReqs, treqs, Seq())

    case If0 =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ
      val (t3, reqs3, treqs3, _) = e.kids(2).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      (t2, mreqs, treqs1 ++ treqs2 ++ treqs3, mcons :+ cond :+ body)

    case Fix =>
      val (t, reqs, treqs, _) = e.kids(0).typ
      val X = freshUVar()
      (X, reqs, treqs, Seq(EqConstraint(t, TFun(X, X))))

    case TAbs =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs, _) = e.kids(0).typ
      (TUniv(alpha, t), reqs, treqs - alpha, Seq())

    case TApp =>
      val (t1, reqs1, treqs, subsol) = e.kids(0).typ
      val t = e.lits(0).asInstanceOf[Type]

      val Xalpha = freshUVar().x
      val Xbody = freshUVar()
      val Xres = freshUVar()

      val ucons = EqConstraint(UUniv(Xalpha, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      (Xres, reqs1, treqs ++ t.freeTVars, Seq(ucons, vcons))
  }

  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) = {
    val (res, time) = Util.timed{
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    }
    localState.stats.mergeReqsTime += time
    res
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

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}