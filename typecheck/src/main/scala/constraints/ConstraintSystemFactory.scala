package constraints

import scala.util.DynamicVariable

abstract class ConstraintSystemFactory[V <: Type, Constraint, CSet <: ConstraintSystem[CSet, Constraint]] {
  val state: DynamicVariable[State[V]] = new DynamicVariable[State[V]](null)

  def freshState: State[V]
  def freshConstraintSystem: CSet
}
