package incremental.pcf.with_references

import incremental.ConstraintOps._
import incremental.{EqConstraint, Type}
import incremental.Type.Companion.TSubst
import incremental.pcf.UVar

/**
 * Created by seba on 15/11/14.
 */
case class TRef(t: Type) extends Type {
  def freeTVars = t.freeTVars
  def occurs(x: Symbol) = t.occurs(x)
  def subst(s: TSubst) = TRef(t.subst(s))
  def unify(other: Type, s: TSubst) = other match {
    case TRef(t2) => t.unify(t2, s)
    case UVar(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

case object TUnit extends Type {
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def subst(s: TSubst) = TUnit
  def unify(other: Type, s: TSubst) = other match {
    case TUnit => emptyCSet
    case UVar(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}