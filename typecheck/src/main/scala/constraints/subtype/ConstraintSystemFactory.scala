package constraints.subtype

import constraints.subtype.CSubst.CSubst
import constraints.{State, Statistics}

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Gen, Constraint, CS] {
  def freshState = new State(new Gen, new Statistics)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
  def solved(s: CSubst): CS
  def notyet(c: Constraint): CS
  def never(c: Constraint): CS
}




