package constraints

import util.NotSoDynamicVariable

abstract class ConstraintSystemFactory[G <: GenBase, C, CS <: ConstraintSystem[G, C, CS]] {
  val state: NotSoDynamicVariable[State[G]] = new NotSoDynamicVariable[State[G]](null)

  def freshState: State[G]
  def freshConstraintSystem: CS
}
