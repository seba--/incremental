package constraints

import scala.util.DynamicVariable

abstract class ConstraintSystemFactory[T <: Type, G <: GenBase, Constraint, CS <: ConstraintSystem[CS, Constraint, T, G]] {
  val state: DynamicVariable[State[G]] = new DynamicVariable[State[G]](null)

  def freshState: State[G]
  def freshConstraintSystem: CS
}
