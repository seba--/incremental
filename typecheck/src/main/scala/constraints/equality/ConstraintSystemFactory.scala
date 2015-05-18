package constraints.equality

import constraints.{Statistics, State}

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Gen, Constraint, CS] {
  def freshState = new State(new Gen, Statistics())
  def freshThreadsafeState = new State(new Gen, Statistics())

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
}




