package incremental.systemfomega

import constraints.CVar
import constraints.normequality.Type.Companion._
import constraints.normequality.{ConstraintSystem, Constraint}

trait Kind {
//  def occurs(x: Symbol): Boolean
}

case class KUvar(x: CVar) extends Kind
case object KStar extends Kind
case class KArrow(k1: Kind, k2: Kind) extends Kind


case class EqKindConstraint(k1: Kind, k2: Kind) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.never(this)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)
  def subst(s: TSubst) = this
}