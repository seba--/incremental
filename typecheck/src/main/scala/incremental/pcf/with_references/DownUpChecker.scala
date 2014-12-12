package incremental.pcf.with_references

import incremental.ConstraintOps._
import incremental.Type._
import incremental._

/**
 * Created by seba on 15/11/14.
 */
trait DownUpChecker extends pcf.DownUpChecker {
  import constraint._

  override def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Ref =>
      val (t, subsol) = typecheck(e.kids(0), ctx)
      (TRef(t), subsol)
    case Deref =>
      val (t1, subsol) = typecheck(e.kids(0), ctx)
      val X = freshTVar()
      val sol = solve(EqConstraint(TRef(X), t1), subsol)
      (X.subst(sol.substitution), sol)
    case Assign =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val subsol = sol1 ++ sol2

      val refcons = EqConstraint(t1, TRef(t2))
      val sol = solve(refcons, subsol)
      (TUnit, sol)
    case Seq =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val subsol = sol1 ++ sol2

      val t1cons = EqConstraint(TUnit, t1)
      val sol = solve(t1cons, subsol)
      (t2.subst(sol.substitution), sol)
    case _ => super.typecheck(e, ctx)
  }
}

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  object PCFRefDownUpChecker extends DownUpChecker
  def makeChecker = PCFRefDownUpChecker
}