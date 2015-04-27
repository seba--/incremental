package constraints.equality

import constraints.State
import constraints.Statistics
import constraints.equality.Type.Companion.TSubst

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Type[CS], UVar[CS], EqConstraint[CS], CS] {
  def system(s: TSubst[CS], notyet: Seq[EqConstraint[CS]], never: Seq[EqConstraint[CS]]): CS

  def freshState = new State(new Gen[CS](this), new Statistics)
  def freshConstraintSystem = system(Map(), Seq(), Seq())

  val emptySolution = system(Map(), Seq(), Seq())
  def solved(s: TSubst[CS]): CS = system(s, Seq(), Seq())
  def notyet(c: EqConstraint[CS]): CS = system(Map(), Seq(c), Seq())
  def never(c: EqConstraint[CS]): CS = system(Map(), Seq(), Seq(c))
}




