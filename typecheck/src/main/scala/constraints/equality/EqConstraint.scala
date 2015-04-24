package constraints.equality

import Type.Companion._

case class EqConstraint(expected: Type, actual: Type) {
  def solve(s: ConstraintSystem) = expected.unify(actual, s.substitution)
  def finalize(s: ConstraintSystem) = solve(s)
  def subst(s: TSubst) = EqConstraint(expected.subst(s), actual.subst(s))
}
