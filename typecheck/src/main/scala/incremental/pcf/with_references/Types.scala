package incremental.pcf.with_references

import incremental.ConstraintOps._
import incremental.{EqConstraint, Type}
import incremental.Type.TSubst
import incremental.pcf.TVar

/**
 * Created by seba on 15/11/14.
 */
case class TRef(t: Type) extends Type {
  val isGround = t.isGround
  def occurs(x: Symbol) = t.occurs(x)
  def subst(s: TSubst) = TRef(t.subst(s))
  def unify(other: Type, s: TSubst) = other match {
    case TRef(t2) => t.unify(t2, s)
    case TVar(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

case object TUnit extends Type {
  val isGround = true
  def occurs(x: Symbol) = false
  def subst(s: TSubst) = TUnit
  def unify(other: Type, s: TSubst) = other match {
    case TUnit => emptySol
    case TVar(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}