package incremental.systemfomega

import constraints.normequality.Type.Companion._
import constraints.normequality.{ConstraintSystem, Constraint}

trait Kind

case class KUvar(x: Symbol) extends Kind
case object KStar extends Kind
case class KArrow(k1: Kind, k2: Kind) extends Kind


case class EqKindConstraint(k1: Kind, k2: Kind) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.never(this)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)
  def subst(s: TSubst) = this
}