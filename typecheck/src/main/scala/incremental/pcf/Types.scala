package incremental.pcf

import incremental.{EqConstraint, Type}
import incremental.ConstraintOps._
import incremental.Type.Companion.TSubst

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
  def unify(other: Type, s: TSubst) = other match {
    case TNum => emptyCSet
    case UVar(x) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

case class UVar(x: Symbol) extends Type {
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def subst(s: TSubst) = s.getOrElse(x, this)
  def unify(other: Type, s: TSubst) =
    if (other == this) emptyCSet
    else s.get(x) match {
      case Some(t) => t.unify(other, s)
      case None =>
        val t = other.subst(s)
        if (this == t)
          emptyCSet
        else if (t.occurs(x))
          never(EqConstraint(this, t))
        else
          solution(Map(x -> t))
    }
}

case class TFun(t1: Type, t2: Type) extends Type {
  def freeTVars = t1.freeTVars ++ t2.freeTVars
  def occurs(x: Symbol) = t1.occurs(x) || t2.occurs(x)
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
  def unify(other: Type, s: TSubst) = other match {
    case TFun(t1_, t2_) =>
      val CSet(s1, _, never1) = t1.unify(t1_, s)
      val CSet(s2, _, never2) = t2.unify(t2_, s1 ++ s)
      CSet(s1 ++ s2, Seq(), never1 ++ never2)
    case UVar(x) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}
