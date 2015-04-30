package incremental.systemf

import constraints.equality._
import constraints.equality.Type.Companion.TSubst

/**
 * Created by seba on 13/11/14.
 */

case object TNum extends Type {
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def normalize = this
  def subst(s: TSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, s: Map[Symbol, Type])(implicit csf: ConstraintSystemFactory[CS]) = other match {
    case TNum => csf.emptySolution
    case UVar(x) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
}

case class TFun(t1: Type, t2: Type) extends Type {
  def freeTVars = t1.freeTVars ++ t2.freeTVars
  def occurs(x: Symbol) = t1.occurs(x) || t2.occurs(x)
  def normalize = TFun(t1.normalize, t2.normalize)
  def subst(s: Map[Symbol, Type]) = TFun(t1.subst(s), t2.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, s: TSubst)(implicit csf: ConstraintSystemFactory[CS]) = other match {
    case TFun(t1_, t2_) =>
      val cs1 = t1.unify(t1_, s)
      val cs2 = t2.unify(t2_, cs1.substitution ++ s)
      cs1 mergeSubsystem cs2
    case UVar(x) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}


case class TVar(alpha : Symbol) extends Type {
  def freeTVars = Set(alpha)
  def normalize = this
  def occurs(x2: Symbol) = alpha == x2
  def subst(s: TSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, s :TSubst)(implicit csf: ConstraintSystemFactory[CS]) = other match {
      case TVar(`alpha`) => csf.emptySolution
    case UVar(x) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
}

case class TUniv(alpha : Symbol, t : Type) extends Type {
  def freeTVars = t.freeTVars - alpha
  def occurs(x2: Symbol) = alpha == x2 || t.occurs(x2)
  def normalize = TUniv(alpha, t.normalize)
  def subst(s: Map[Symbol, Type]) = TUniv(alpha, t.subst(s - alpha))
  def unify[CS <: ConstraintSystem[CS]](other: Type, s :TSubst)(implicit csf: ConstraintSystemFactory[CS]) = other match {
    case UUniv(alpha2, t2) => csf.solved(Map(alpha2 -> TVar(alpha))) mergeSubsystem t.unify(t2, s + (alpha2 -> TVar(alpha)))
    case TUniv(alpha2, t2) => t.unify(t2, s + (alpha2 -> TVar(alpha)))
    case UVar(_) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
}

case class UUniv(alpha: Symbol, t : Type) extends Type {
  def freeTVars = t.freeTVars
  def normalize = UUniv(alpha, t.normalize)
  def occurs(x2: Symbol) = alpha == x2 || t.occurs(x2)
  def subst(s : Map[Symbol, Type]) = s.get(alpha) match {
    case Some(TVar(beta)) => TUniv(beta, t.subst(s))
    case Some(UVar(beta)) => UUniv(beta, t.subst(s))
    case None => UUniv(alpha, t.subst(s))
    case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, s :TSubst)(implicit csf: ConstraintSystemFactory[CS]) = other match {
    case UUniv(alpha2, t2) => csf.solved(Map(alpha -> UVar(alpha2))) mergeSubsystem t.unify(t2, s + (alpha -> UVar(alpha2)))
    case TUniv(alpha2, t2) => csf.solved(Map(alpha -> TVar(alpha2))) mergeSubsystem t.unify(t2, s + (alpha -> TVar(alpha2)))
    case UVar(_) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
}
