package incremental.pcf.with_records

import incremental.Type._
import incremental.pcf.EqConstraint
import incremental.{TypeCheckerFactory, Exp_, pcf}

/**
* Created by seba on 15/11/14.
*/
trait DownUpChecker extends pcf.DownUpChecker {

  override val constraint = new ConstraintOps
  import constraint._

  override def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Record =>
      val keys = e.lits(0).asInstanceOf[Seq[Symbol]]

      var sol = emptySol
      val subs = for (sub <- e.kids.seq) yield {
        val (t, s, unres) = typecheck(sub, ctx)
        sol = mergeSolution(sol, (s, unres))
        t
      }

      val fields = keys.zip(subs).toMap
      (TRecord(fields), sol._1, sol._2)

    case Project =>
      val label = e.lits(0).asInstanceOf[Symbol]
      val (t1, s1, unres1) = typecheck(e.kids(0), ctx)
      val X = freshTVar()
      val (s, unres) = solve(RecordProjectConstraint(t1, label, X), (s1, unres1))
      (X, s, unres)

    case _ => super.typecheck(e, ctx)
  }
}

object DownUpCheckerFactory extends TypeCheckerFactory {
  object PCFRefDownUpChecker extends DownUpChecker
  def makeChecker = PCFRefDownUpChecker
}