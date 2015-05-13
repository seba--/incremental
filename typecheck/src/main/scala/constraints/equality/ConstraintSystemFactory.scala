package constraints.equality

import constraints.{Statistics, State}

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Gen, Constraint, CS] {
  def freshState = new State(new Gen, new Statistics)
  def freshThreadsafeState = new State(new Gen, new Statistics)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
}




