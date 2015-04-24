package constraints.equality

import constraints.State
import constraints.Statistics
import constraints.equality.Type._

object ConstraintSystemFactory extends constraints.ConstraintSystemFactory[Type, EqConstraint, ConstraintSystem] {
  type Constraint = EqConstraint
  type NotYetSolvable = Seq[Constraint]
  type Unsolvable = Seq[Constraint]

  def freshState = new State(new Gen, new Statistics)
  def freshConstraintSystem = ConstraintSystem(Map(), Seq(), Seq())
}




