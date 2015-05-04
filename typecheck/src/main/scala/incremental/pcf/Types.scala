package incremental.pcf

import constraints.CVar
import constraints.equality.{ConstraintSystem, ConstraintSystemFactory, EqConstraint, Type}
import constraints.equality.Type.Companion._

/**
 * Created by seba on 13/11/14.
 */
//trait CheckResult {
//  (Type, Map[Symbol, Type], Unsolvable)
//}

case class UVar(x: CVar) extends Type {
  def occurs(x2: CVar) = x == x2
  def subst(s: TSubst) = s.getOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (other == this) cs
    else cs.substitution.get(x) match {
      case Some(t) => t.unify(other, cs)
      case None =>
        val t = other.subst(cs.substitution)
        if (this == t)
          cs
        else if (t.occurs(x))
          cs.never(EqConstraint(this, t))
        else
          cs.solved(Map(x -> t))
    }
}

case object TNum extends Type {
  def occurs(x: CVar) = false
  def subst(s: TSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TNum => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class TFun(t1: Type, t2: Type) extends Type {
  def occurs(x: CVar) = t1.occurs(x) || t2.occurs(x)
  def subst(s: TSubst) = {
    var args = List(t1.subst(s))
    var res = t2
    while (res.isInstanceOf[TFun]) {
      val resfun = res.asInstanceOf[TFun]
      args = resfun.t1.subst(s) :: args
      res = resfun.t2
    }
    res = res.subst(s)
    for (a <- args)
      res = TFun(a, res)
    res
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TFun(t1_, t2_) => t2.unify(t2_, t1.unify(t1_, cs))
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}
