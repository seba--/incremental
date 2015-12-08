package incremental.systemf

import constraints.Statistics
import constraints.equality._
import incremental.{Node_, Util}

/**
 * Created by qwert on 08.12.15.
 */
object SystemFCheck {
  type Reqs = Map[Symbol, Type]
  type TReqs = Set[Symbol]
  type Result = (Type, Reqs, TReqs)
  type Kid = Node_[Constraint, _, Result]

  def emptyReqs: Reqs = Map()
  def emptyTReqs: TReqs = Set()

  val gen = new Gen
  def freshUVar() = UVar(gen.freshSymbol[Type]("T"))

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
          mcons = EqConstraint(r1, r2) +: mcons
      }
    (mcons, mreqs)
  }
}
