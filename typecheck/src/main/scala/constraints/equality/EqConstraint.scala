package constraints.equality

import Type.Companion._

case class EqConstraint[CS <: ConstraintSystem[CS]](expected: Type[CS], actual: Type[CS]) {
  def solve(s: ConstraintSystem[CS]): CS = expected.unify(actual, s.substitution)
  def finalize(s: ConstraintSystem[CS]): CS = solve(s)
  def subst(s: TSubst[CS]): EqConstraint[CS] = EqConstraint(expected.subst(s), actual.subst(s))
}
