package constraints.equality

import constraints.State
import constraints.Statistics
import constraints.equality.Type.Companion.TSubst

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Type, UVar, Gen, Constraint, CS] {
  def freshState = new State(new Gen, new Statistics)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
}




