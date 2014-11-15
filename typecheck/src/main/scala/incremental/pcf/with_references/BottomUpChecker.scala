package incremental.pcf.with_references

import incremental.pcf.EqConstraint
import incremental.{TypeCheckerFactory, Exp_, pcf}

/**
 * Created by seba on 15/11/14.
 */
trait BottomUpChecker extends pcf.BottomUpChecker {

  import constraint._

  override def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Ref =>
      val (t, reqs, unres) = e.kids(0).typ
      (TRef(t), reqs, unres)
    case Deref =>
      val (t, reqs, unres) = e.kids(0).typ
      val X = freshTVar()
      val (s, newunres) = solve(EqConstraint(TRef(X), t))
      (X.subst(s), reqs.mapValues(_.subst(s)), unres ++ newunres)
    case Assign =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ

      val refcons = EqConstraint(t1, TRef(t2))
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val (s, newunres) = solve(refcons +: mcons)
      (TUnit, mreqs.mapValues(_.subst(s)), unres1 ++ unres2 ++ newunres)
    case Seq =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ

      val t1cons = EqConstraint(TUnit, t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val (s, newunres) = solve(t1cons +: mcons)
      (t2, mreqs.mapValues(_.subst(s)), unres1 ++ unres2 ++ newunres)
    case _ => super.typecheckStep(e)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory {
  object PCFRefBottomUpChecker extends BottomUpChecker
  def makeChecker = PCFRefBottomUpChecker
}