package constraints.javacons

import constraints.javacons.CSubst.CSubst

/**
 * Created by qwert on 22.07.15.
 */

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[Gen, Constraint, CS] {
  def substitution: CSubst

  def tryFinalize: ConstraintSystem[_]
}
