package incremental.pcf.with_subtyping

import constraints.Statistics
import constraints.subtype._
import incremental.{Node_, Util}

/**
 * Created by qwert on 11.12.15.
 */
object SubtypingCheck {
  type Reqs = Map[Symbol, Type]
  type Result = (Type, Reqs)
  type Kid = Node_[Constraint, _, Result]

  val emptyReqs: Reqs = Map()

  val gen = new Gen

  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    //Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    //}

  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          val Xmeet = gen.freshUVar(false)
          mcons = Meet(Xmeet, Set(r1, r2)) +: mcons
          mreqs += x -> Xmeet
      }
    (mcons, mreqs)
  }
}
