package constraints.normequality

import Type.Companion._

trait Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS
  def subst(s: TSubst): Constraint
}

case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = expected.unify(actual, cs)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
  def subst(s: TSubst): EqConstraint = EqConstraint(expected.subst(s), actual.subst(s))
}
