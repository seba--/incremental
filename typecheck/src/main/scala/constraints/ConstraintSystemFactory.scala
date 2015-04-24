package constraints

import scala.util.DynamicVariable

abstract class ConstraintSystemFactory[T <: Type, Constraint, CSet <: ConstraintSystem[CSet, Constraint]] {
  val state: DynamicVariable[State[T]] = new DynamicVariable[State[T]](null)

  def freshState: State[T]
  def freshConstraintSystem: CSet
}
