package incremental.systemf

import incremental.{TypCompanion, UType, EqConstraint, Type}
import incremental.ConstraintOps._
import incremental.Type.Companion._

/**
 * Created by seba on 13/11/14.
 */
//trait CheckResult {
//  (Type, Map[Symbol, Type], Unsolvable)
//}

case object TNum extends Type {
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def subst(s: TSubst) = this
  def unify(other: Type, s: Map[Symbol, Type]) = other match {
    case TNum => emptySol
    case UVar(x) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

case class TFun(t1: Type, t2: Type) extends Type {
  def freeTVars = t1.freeTVars ++ t2.freeTVars
  def occurs(x: Symbol) = t1.occurs(x) || t2.occurs(x)
  def subst(s: Map[Symbol, Type]) = TFun(t1.subst(s), t2.subst(s))
  def unify(other: Type, s: TSubst) = other match {
    case TFun(t1_, t2_) =>
      val sol1 = t1.unify(t1_, s)
      val sol2 = t2.unify(t2_, s)
      sol1 ++ sol2
    case UVar(x) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}


case class TVar(alpha : Symbol) extends Type {
  def freeTVars = Set(alpha)

  def occurs(x2: Symbol) = alpha == x2

  def subst(s: TSubst) = this//subst(s + (alpha -> TUsVar(alpha)))
  // def subst(s: Map[Symbol, Type]) =  //s.apply(alpha) //(Map(alpha -> TUsVar(alpha))
  def unify(other: Type, s :TSubst) = other match {
      case TVar(`alpha`) => emptySol
    case UVar(x) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

case class UVar(x: Symbol) extends Type {
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def subst(s: Map[Symbol, Type]) = s.getOrElse(x, this)
  def unify(other: Type, s: TSubst) =
    if (other == this) emptySol
    else s.get(x) match {
      case Some(t) => t.unify(other, s)
      case None =>
        val t = other.subst(s)
        if (this == t)
          emptySol
        else if (t.occurs(x))
          never(EqConstraint(this, t))
        else
          solution(Map(x -> t))
    }
}

case class TUniv(alpha : Symbol, t : Type) extends Type {
  def freeTVars = t.freeTVars - alpha

  def occurs(x2: Symbol) = alpha == x2 || t.occurs(x2

  def subst(s: Map[Symbol, Type]) = TUniv(alpha, t.subst(s - alpha))

  def unify(other: Type, s: TSubst) = other match {
    case UUniv(alpha2, t2) => solution(Map(alpha2 -> TVar(alpha))) ++ t.unify(t2, s + (alpha2 -> TVar(alpha)))
    case TUniv(alpha2, t2) => t.unify(t2, s + (alpha2 -> TVar(alpha)))
    case UVar(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

case class UUniv(alpha: Symbol, t : Type) extends Type {
  def freeTVars = t.freeTVars

  def occurs(x2: Symbol) = alpha == x2 || t.occurs(x2

  def subst(s : Map[Symbol, Type]) = s.get(alpha) match {
    case Some(TVar(beta)) => TUniv(beta, t.subst(s))
    case Some(UVar(beta)) => UUniv(beta, t.subst(s))
    case None => UUniv(alpha, t.subst(s))
    case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
  }

  def unify(other: Type, s: TSubst) = other match {
    case UUniv(alpha2, t2) => solution(Map(alpha -> UVar(alpha2))) ++ t.unify(t2, s + (alpha -> UVar(alpha2)))
    case TUniv(alpha2, t2) => solution(Map(alpha -> TVar(alpha2))) ++ t.unify(t2, s + (alpha -> TVar(alpha2)))
    case UVar(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}
