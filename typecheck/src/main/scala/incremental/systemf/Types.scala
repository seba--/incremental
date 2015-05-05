package incremental.systemf

import constraints.CVar
import constraints.equality.CSubst.CSubst
import constraints.equality._

/**
 * Created by seba on 13/11/14.
 */

trait PolType extends constraints.equality.Type {
  def freeTVars: Set[Symbol]
  def getFreeTVars(t: Type): Set[Symbol] =
    if (t.isInstanceOf[PolType])
      t.asInstanceOf[PolType].freeTVars
    else
      Set()
}

case class UVar(x: CVar[Type]) extends PolType {
  def freeTVars = Set()
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst) = s.hgetOrElse(x, this)
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

case object TNum extends PolType {
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TNum => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class TFun(t1: Type, t2: Type) extends PolType {
  def freeTVars = getFreeTVars(t1) ++ getFreeTVars(t2)
  def occurs(x: CVar[_]) = t1.occurs(x) || t2.occurs(x)
  def subst(s: CSubst) = TFun(t1.subst(s), t2.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TFun(t1_, t2_) => t2.unify(t2_, t1.unify(t1_, cs))
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}


case class TVar(alpha : Symbol) extends PolType {
  def freeTVars = Set(alpha)
  def occurs(x2: CVar[_]) = alpha == x2
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case TVar(`alpha`) => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class TUniv(alpha : Symbol, t : Type) extends PolType {
  def freeTVars = getFreeTVars(t) - alpha
  def occurs(x2: CVar[_]) = t.occurs(x2)
  def subst(s: CSubst) = TUniv(alpha, t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case UUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(alpha2 -> TVar(alpha))))
    case TUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(CVar[Type](alpha2) -> TVar(alpha)))) without Set(CVar(alpha2))
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class UUniv(alpha: CVar[Type], t : Type) extends PolType {
  def freeTVars = getFreeTVars(t)
  def occurs(x2: CVar[_]) = alpha == x2 || t.occurs(x2)
  def subst(s : CSubst) = s.hget(alpha) match {
    case Some(TVar(beta)) => TUniv(beta, t.subst(s))
    case Some(UVar(beta)) => UUniv(beta, t.subst(s))
    case None => UUniv(alpha, t.subst(s))
    case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case UUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(alpha -> UVar(alpha2))))
    case TUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(alpha -> TVar(alpha2))))
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
