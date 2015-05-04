package incremental.pcf.with_references

import constraints.CVar
import constraints.equality._
import constraints.equality.Type.Companion.TSubst
import incremental.pcf.UVar

/**
 * Created by seba on 15/11/14.
 */
case class TRef(t: Type) extends Type {
  def occurs(x: CVar) = t.occurs(x)
  def subst(s: TSubst) = TRef(t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TRef(t2) => t.unify(t2, cs)
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TUnit extends Type {
  def occurs(x: CVar) = false
  def subst(s: TSubst) = TUnit
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TUnit => cs
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}