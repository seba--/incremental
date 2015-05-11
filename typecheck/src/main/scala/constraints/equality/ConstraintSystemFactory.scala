package constraints.equality

import constraints.{LocalStatistics, ThreadedStatistics, State}

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Gen, Constraint, CS] {
  def freshState = new State(new Gen, new LocalStatistics)
  def freshThreadsafeState = new State(new Gen, new ThreadedStatistics)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
}




