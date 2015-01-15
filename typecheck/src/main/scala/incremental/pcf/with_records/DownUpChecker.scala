package incremental.pcf.with_records

import incremental.Type.Companion
import incremental.{Type, TypeCheckerFactory, Exp_, pcf}
import incremental.ConstraintOps._

/**
* Created by seba on 15/11/14.
*/
trait DownUpChecker extends pcf.DownUpChecker {

  import cs._
  import localState.gen._


  override def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Record =>
      val keys = e.lits.asInstanceOf[Seq[Symbol]]

      var sol = emptyCSet
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
      val X = freshUVar()
      val sol = subsol + EqRecordProjectConstraint(t1, label, X)
      (X.subst(sol.substitution), sol)

    case _ => super.typecheck(e, ctx)
  }
}

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  object PCFRefDownUpChecker extends DownUpChecker
  def makeChecker = PCFRefDownUpChecker
}