package incremental.pcf.with_records

import incremental.Type._
import incremental.{TypeCheckerFactory, Exp_, pcf}
import incremental.ConstraintOps._

/**
* Created by seba on 15/11/14.
*/
trait DownUpChecker extends pcf.DownUpChecker {

  override val constraint = new ConstraintOps
  import constraint._

  override def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Record =>
      val keys = e.lits.asInstanceOf[Seq[Symbol]]

      var sol = emptySol
      val subs = for (sub <- e.kids.seq) yield {
        val (t, kidsol) = typecheck(sub, ctx)
        sol = sol ++ kidsol
        t
      }

      val fields = keys.zip(subs).toMap
      (TRecord(fields), sol)

    case Project =>
      val label = e.lits(0).asInstanceOf[Symbol]
      val (t1, subsol) = typecheck(e.kids(0), ctx)
      val X = freshTVar()
      val sol = solve(EqRecordProjectConstraint(t1, label, X), subsol)
      (X.subst(sol.solution), sol)

    case _ => super.typecheck(e, ctx)
  }
}

object DownUpCheckerFactory extends TypeCheckerFactory {
  object PCFRefDownUpChecker extends DownUpChecker
  def makeChecker = PCFRefDownUpChecker
}