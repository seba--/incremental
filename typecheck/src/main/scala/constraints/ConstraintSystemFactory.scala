package constraints

import scala.util.DynamicVariable

abstract class ConstraintSystemFactory[T <: Type, V <: T, Constraint, CS <: ConstraintSystem[CS, Constraint, T]] {
  val state: DynamicVariable[State[V]] = new DynamicVariable[State[V]](null)

  def freshState: State[V]
  def freshConstraintSystem: CS
}
