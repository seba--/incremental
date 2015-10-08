package incremental.spreadsheet

import constraints.equality.CSubst
import constraints.equality.CSubst.CSubst
import constraints.CVar
import constraints.equality.{ConstraintSystem, ConstraintSystemFactory, EqConstraint, Type}

/**
 * Created by seba on 13/11/14.
 */

case class UVar(x: CVar[Type]) extends Type {
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst):Type = s.hgetOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (other == this) cs
    else cs.substitution.hget(x) match {
      case Some(t) => t.unify(other, cs)
      case None =>
        val t = other.subst(cs.substitution)
        if (this == t)
          cs
        else if (t.occurs(x))
          cs.never(EqConstraint(this, t))
        else
          cs.solved(CSubst(x -> t))
    }
}

case object TEmpty extends Type {
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TEmpty => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TInt extends Type {
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TInt => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TString extends Type {
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TString => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
