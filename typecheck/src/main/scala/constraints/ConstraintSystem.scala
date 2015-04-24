package constraints

import incremental.Util

import scala.util.DynamicVariable

abstract class ConstraintSystem[
  Type <: Typ[Type],
  Constraint,
  Requirements,
  Solution,
  CSet <: CSetAlg[CSet, Constraint, Solution],
  Gen <: GenBase[Type]
](implicit val definitions: TypCompanion[Type]) {

  final type TSubst = definitions.TSubst

  def freshState: State[Type]
  val state: DynamicVariable[State[Type]] = new DynamicVariable[State[Type]](null)

  def emptyCSet: CSet
  def solution(s: TSubst): CSet
  def notyet(c: Constraint): CSet
  def never(c: Constraint): CSet

  final def mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[Constraint], Requirements) = {
    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
    state.value.stats.mergeReqsTime += time
    res
  }

  def _mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[Constraint], Requirements)
}
