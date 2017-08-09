package constraints.fjavaMO

import constraints.State
import constraints.fjavaMO.CSubst.CSubst

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Gen, Constraint, CS] {
  def freshState = new State(new Gen)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
}




