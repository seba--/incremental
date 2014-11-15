package incremental.pcf.with_references

import incremental.Type
import incremental.Type.TSubst
import incremental.pcf.TVar

/**
 * Created by seba on 15/11/14.
 */
case class TRef(t: Type) extends Type {
  def subst(s: TSubst) = TRef(t.subst(s))
  def unify(other: Type, s: TSubst) = other match {
    case TRef(t2) => t.unify(t2, s)
    case TVar(_) => other.unify(this, s)
    case _ => None
  }
}

case object TUnit extends Type {
  def subst(s: TSubst) = TUnit
  def unify(other: Type, s: TSubst) = other match {
    case TUnit => Some(Map())
    case TVar(_) => other.unify(this, s)
    case _ => None
  }
}