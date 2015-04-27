package constraints.equality

import constraints.State
import constraints.Statistics
import constraints.equality.Type.Companion.TSubst

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Type, UVar, EqConstraint, CS] {
  def freshState = new State(new Gen, new Statistics)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
  def solved(s: TSubst): CS
  def notyet(c: EqConstraint): CS
  def never(c: EqConstraint): CS
}




