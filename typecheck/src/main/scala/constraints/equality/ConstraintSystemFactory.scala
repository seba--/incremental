package constraints.equality

import constraints.State
import constraints.Statistics
import constraints.equality.Type.Companion.TSubst

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Type[CS], UVar[CS], EqConstraint[CS], CS] {
  def freshState = new State(new Gen[CS](this), new Statistics)

  def freshConstraintSystem: CS
  val emptySolution: CS
  def solved(s: TSubst[CS]): CS
  def notyet(c: EqConstraint[CS]): CS
  def never(c: EqConstraint[CS]): CS
}




