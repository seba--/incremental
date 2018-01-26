package incremental.pcf.with_references

import constraints.CVar
import constraints.equality.CSubst.CSubst
import constraints.equality._
import incremental.pcf.UVar

/**
 * Created by seba on 15/11/14.
 */
case class TRef(t: Type) extends Type {
  def isGround = false
  def occurs(x: CVar[_]) = t.occurs(x)
  def subst(s: CSubst) = TRef(t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TRef(t2) => t.unify(t2, cs)
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TUnit extends Type {
  def isGround = true
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = TUnit
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TUnit => cs
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}