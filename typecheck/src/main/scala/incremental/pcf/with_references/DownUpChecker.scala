package incremental.pcf.with_references

import incremental.Type._
import incremental.pcf.EqConstraint
import incremental.{TypeCheckerFactory, Exp_, pcf}

/**
 * Created by seba on 15/11/14.
 */
trait DownUpChecker extends pcf.DownUpChecker {

  import constraint._

  override def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Ref =>
      val (t, s, unres) = typecheck(e.kids(0), ctx)
      (TRef(t), s, unres)
    case Deref =>
      val (t1, s1, unres1) = typecheck(e.kids(0), ctx)
      val X = freshTVar()
      val (s, unres) = solve(EqConstraint(TRef(X), t1), (s1, unres1))
      (X.subst(s), s, unres)
    case Assign =>
      val (t1, s1, unres1) = typecheck(e.kids(0), ctx)
      val (t2, s2, unres2) = typecheck(e.kids(1), ctx)
      val subsol = mergeSolution((s1, unres1), (s2, unres2))

      val refcons = EqConstraint(t1, TRef(t2))
      val (s, unres) = solve(refcons, subsol)
      (TUnit, s, unres)
    case Seq =>
      val (t1, s1, unres1) = typecheck(e.kids(0), ctx)
      val (t2, s2, unres2) = typecheck(e.kids(1), ctx)
      val subsol = mergeSolution((s1, unres1), (s2, unres2))

      val t1cons = EqConstraint(TUnit, t1)
      val (s, unres) = solve(t1cons, subsol)
      (t2, s, unres)
    case _ => super.typecheck(e, ctx)
  }
}

object DownUpCheckerFactory extends TypeCheckerFactory {
  object PCFRefDownUpChecker extends DownUpChecker
  def makeChecker = PCFRefDownUpChecker
}