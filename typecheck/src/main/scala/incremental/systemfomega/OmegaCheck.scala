package incremental.systemfomega

import constraints.Statistics
import constraints.normequality._
import incremental.{Util, Node_}

/**
 * Created by qwert on 09.12.15.
 */
object OmegaCheck {
  type Reqs = Map[Symbol, Type]
  type TReqs = Map[Symbol, Kind]
  type Kid = Node_[Constraint, _, Result]

  val emptyReqs: Reqs = Map()
  val emptyTReqs: TReqs = Map()

  trait Result
  case class ExpResult(t: Type, reqs: Reqs, treqs: TReqs) extends Result
  case class TypeResult(k: Kind, treqs: TReqs) extends Result

  val gen = new Gen
  def freshUVar() = UVar(gen.freshSymbol("x$"))
  def freshKUVar() = KUvar(gen.freshSymbol("K$"))

  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())
  private val tinit: (Seq[Constraint], TReqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)
  def mergeTReqMaps(req: TReqs, reqs: TReqs*): (Seq[Constraint], TReqs) = mergeTReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    //Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps(EqConstraint))
    //}

  def mergeTReqMaps(reqs: Seq[TReqs]): (Seq[Constraint], TReqs) =
    //Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Seq[Constraint], TReqs)](tinit)(_mergeReqMaps(EqKindConstraint))
    //}

  private def _mergeReqMaps[K, V](eqC: (V,V) => Constraint)(was: (Seq[Constraint], Map[K,V]), newReqs: Map[K,V]) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = eqC(r1, r2) +: mcons
      }
    (mcons, mreqs)
  }
}
