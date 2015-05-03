package constraints

import scala.util.DynamicVariable

abstract class ConstraintSystemFactory[T <: Type, V <: T, G <: GenBase[V], Constraint, CS <: ConstraintSystem[CS, Constraint, T, V, G]] {
  val state: DynamicVariable[State[V, G]] = new DynamicVariable[State[V, G]](null)

  def freshState: State[V, G]
  def freshConstraintSystem: CS
}
