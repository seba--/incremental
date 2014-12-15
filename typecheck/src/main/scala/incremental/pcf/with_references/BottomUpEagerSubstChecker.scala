package incremental.pcf.with_references

import incremental.ConstraintOps._
import incremental._

/**
 * Created by seba on 15/11/14.
 */
trait BottomUpEagerSubstChecker extends pcf.BottomUpEagerSubstChecker {

  import constraint._

  override def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Ref =>
      val (t, reqs, subsol) = e.kids(0).typ
      (TRef(t), reqs, subsol)
    case Deref =>
      val (t, reqs, subsol) = e.kids(0).typ
      val X = freshUVar()
      val sol = solve(EqConstraint(TRef(X), t))
      (X.subst(sol.substitution), reqs.mapValues(_.subst(sol.substitution)), subsol <++ sol)
    case Assign =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ

      val refcons = EqConstraint(t1, TRef(t2))
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = solve(refcons +: mcons)
      (TUnit, mreqs.mapValues(_.subst(sol.substitution)), sol1 +++ sol2 <++ sol)
    case Seq =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ

      val t1cons = EqConstraint(TUnit, t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = solve(t1cons +: mcons)
      (t2, mreqs.mapValues(_.subst(sol.substitution)), sol1 +++ sol2 <++ sol)
    case _ => super.typecheckStep(e)
  }
}

object BottomUpEagerSubstCheckerFactory extends TypeCheckerFactory[Type] {
  object PCFRefBottomUpChecker extends BottomUpEagerSubstChecker
  def makeChecker = PCFRefBottomUpChecker
}