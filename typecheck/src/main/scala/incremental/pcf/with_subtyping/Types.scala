package incremental.pcf.with_subtyping

import incremental.ConstraintOps._
import incremental.{EqConstraint, Type}
import incremental.Type.TSubst
import incremental.pcf.TVar

/**
 * Created by oliver on 19.11.14.
 */
case object Top extends Type {
  def occurs(x: Symbol) = false

  def subst(s: TSubst) = this

  def unify(other: Type, s: TSubst) = other match {
    case Top => emptySol
    case TVar(x) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

case object Bot extends Type {
  def occurs(x: Symbol) = false

  def subst(s: TSubst) = this

  def unify(other: Type, s: TSubst) = ???
}
