package incremental.pcf

import constraints.Statistics
import constraints.equality._
import incremental.{Util, Node_}

/**
 * Created by qwert on 04.12.15.
 */
object PCFCheck {
  type Reqs = Map[Symbol, Type]
  type Result = (Type, Reqs)
  type Kid = Node_[Constraint, _, Result]

  val emptyReqs: Reqs = Map()
  val emptyCons: Seq[Constraint] = Seq()

  val gen = new Gen
  def freshUVar() = UVar(gen.freshSymbol[Type]("T"))

  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    //Util.timed(localState -> Statistics.mergeReqsTime) { // TODO: no localState at this position
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
          mcons = EqConstraint(r1, r2) +: mcons
      }
    (mcons, mreqs)
  }
}
