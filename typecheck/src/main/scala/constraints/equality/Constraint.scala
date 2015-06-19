package constraints.equality

import constraints.equality.CSubst.CSubst

trait Constraint extends constraints.Constraint[Gen, Constraint] {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS
}

case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = expected.unify(actual, cs)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
  def subst(s: CSubst) = EqConstraint(expected.subst(s), actual.subst(s))
}

case class NotEqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS =  cs.never(EqConstraint(expected, actual))  // actual.unify(actual, cs)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
  def subst(s: CSubst) = EqConstraint(expected.subst(s), actual.subst(s))
}
