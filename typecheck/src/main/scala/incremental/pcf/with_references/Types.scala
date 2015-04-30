package incremental.pcf.with_references

import constraints.equality._
import constraints.equality.Type.Companion.TSubst

/**
 * Created by seba on 15/11/14.
 */
case class TRef(t: Type) extends Type {
  def freeTVars = t.freeTVars
  def occurs(x: Symbol) = t.occurs(x)
  def normalize = TRef(t.normalize)
  def subst(s: TSubst) = TRef(t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, s: TSubst)(implicit csf: ConstraintSystemFactory[CS]) = other match {
    case TRef(t2) => t.unify(t2, s)
    case UVar(_) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
}

case object TUnit extends Type {
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def normalize = this
  def subst(s: TSubst) = TUnit
  def unify[CS <: ConstraintSystem[CS]](other: Type, s: TSubst)(implicit csf: ConstraintSystemFactory[CS]) = other match {
    case TUnit => csf.emptySolution
    case UVar(_) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
}