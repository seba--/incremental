package constraints

import scala.util.DynamicVariable

abstract class ConstraintSystemFactory[T <: Type, Constraint, CSet <: ConstraintSystem[CSet, Constraint]] {
  val state: DynamicVariable[State[T]] = new DynamicVariable[State[T]](null)

  def freshState: State[T]
  def freshConstraintSystem: CSet

//  def solution(s: TSubst): CSet
//  def notyet(c: Constraint): CSet
//  def never(c: Constraint): CSet

//  final def mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[Constraint], Requirements) = {
//    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
//    state.value.stats.mergeReqsTime += time
//    res
//  }
//
//  def _mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[Constraint], Requirements)
}
