package incremental.pcf.with_records

import incremental.ConstraintOps._
import incremental._
import Type._

/**
* Created by seba on 15/11/14.
*/
trait BottomUpChecker extends pcf.BottomUpChecker {

  import constraint._

  override def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Record =>
      val keys = e.lits.asInstanceOf[Seq[Symbol]]

      var mcons = Seq[Constraint]()
      var mreqs: Reqs = Map()
      var msol = emptySol
      val subs = for (sub <- e.kids.seq) yield {
        val (t, subreqs, subsol) = sub.typ
        msol = msol +++ subsol
        val (cons, reqs) = mergeReqMaps(subreqs, subreqs)
        mcons = mcons ++ cons
        mreqs = reqs
        t
      }

      val sol = solve(mcons)

      val fields = keys.zip(subs).toMap
      (TRecord(fields), mreqs.mapValues(_.subst(sol.substitution)), msol <++ sol)

    case Project =>
      val label = e.lits(0).asInstanceOf[Symbol]
      val (t1, reqs, subsol) = e.kids(0).typ
      val X = freshUVar()

      val sol = solve(EqRecordProjectConstraint(t1, label, X))

      (X.subst(sol.substitution), reqs.mapValues(_.subst(sol.substitution)), subsol <++ sol)

    case _ => super.typecheckStep(e)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  object PCFRefBottomUpChecker extends BottomUpChecker
  def makeChecker = PCFRefBottomUpChecker
}