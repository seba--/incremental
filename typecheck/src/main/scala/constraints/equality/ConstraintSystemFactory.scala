package constraints.equality

import constraints.State
import constraints.Statistics

object ConstraintSystemFactory extends constraints.ConstraintSystemFactory[Type, UVar, EqConstraint, ConstraintSystem] {
  type Constraint = EqConstraint
  type NotYetSolvable = Seq[Constraint]
  type Unsolvable = Seq[Constraint]

  def freshState = new State(new Gen, new Statistics)
  def freshConstraintSystem = ConstraintSystem(Map(), Seq(), Seq())

  val emptySolution = ConstraintSystem(Map(), Seq(), Seq())
  def solved(s: Type.Companion.TSubst): ConstraintSystem = ConstraintSystem(s, Seq(), Seq())
  def notyet(c: Constraint): ConstraintSystem = ConstraintSystem(Map(), Seq(c), Seq())
  def never(c: Constraint): ConstraintSystem = ConstraintSystem(Map(), Seq(), Seq(c))
}




