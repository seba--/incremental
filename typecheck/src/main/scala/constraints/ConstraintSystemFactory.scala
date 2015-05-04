package constraints

import scala.util.DynamicVariable

abstract class ConstraintSystemFactory[G <: GenBase, C, CS <: ConstraintSystem[G, C, CS]] {
  val state: DynamicVariable[State[G]] = new DynamicVariable[State[G]](null)

  def freshState: State[G]
  def freshConstraintSystem: CS
}
