package constraints

abstract class ConstraintSystemFactory[G <: GenBase, C, CS <: ConstraintSystem[G, C, CS]] {
  var state: State[G] = null

  def freshState: State[G]
  def freshThreadsafeState: State[G]

  def freshConstraintSystem: CS
}
