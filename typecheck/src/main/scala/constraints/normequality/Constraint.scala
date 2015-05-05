package constraints.normequality

import constraints.normequality.CSubst.CSubst

trait Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS
  def subst(s: CSubst): Constraint
}

case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = expected.unify(actual, cs)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
  def subst(s: CSubst): EqConstraint = EqConstraint(expected.subst(s), actual.subst(s))
}
