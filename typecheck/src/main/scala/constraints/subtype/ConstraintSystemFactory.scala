package constraints.subtype

import constraints.{State, Statistics}
import Type.Companion.TSubst

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Type, Gen, Constraint, CS] {
  def freshState = new State(new Gen, new Statistics)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
  def solved(s: TSubst): CS
  def notyet(c: Constraint): CS
  def never(c: Constraint): CS
}




