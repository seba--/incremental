package incremental.pcf_graph

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
  def normalize = this
  def subst(s: TSubst) = this
  def unify(other: Type, s: TSubst) = other match {
    case TNum => emptySol
    case TUnifiable(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}

//case class TVar(alpha : Symbol) extends Type {
//  def freeTVars = Set(alpha)
//
//  def occurs(x2: Symbol) = alpha == x2
//
//  def normalize = this
//  def subst(s: TSubst) = this
//  def unify(other: Type) = other match {
//      case TVar(`alpha`) => emptySol
//      case TUnifiable(_) => other.unify(this)
//      case _ => never(EqConstraint(this, other))
//    }
//}

case class TUnifiable(alpha: Symbol) extends Type {
  var t: Type = null

  def isVar = t == null

  def freeTVars = if (isVar) Set(alpha) else t.freeTVars

  def occurs(x: Symbol) = if (isVar) alpha == x else t.occurs(x)

  def normalize = if (isVar) this else t.normalize

  def subst(s: TSubst) =
    if (isVar) s.get(alpha) match {
      case None => this
      case Some(t2) => t2
    }
    else t.subst(s)

  def unify(other: Type, s: TSubst) =
    if (isVar) {
      if (other == this)
        emptySol
      else if (other.occurs(alpha))
        never(EqConstraint(this, other))
      else {
        this.t = other
        emptySol
      }
    }
    else t.unify(other, s)
}

case class TFun(t1: Type, t2: Type) extends Type {
  def freeTVars = t1.freeTVars ++ t2.freeTVars
  def occurs(x: Symbol) = t1.occurs(x) || t2.occurs(x)
  def normalize = {
    var args = List(t1.normalize)
    var res = t2
    while (res.isInstanceOf[TFun]) {
      val resfun = res.asInstanceOf[TFun]
      args = resfun.t1.normalize :: args
      res = resfun.t2
    }
    res = res.normalize
    for (a <- args)
      res = TFun(a, res)
    res
  }
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
      val Solution(_, _, never1) = t1.unify(t1_, s)
      val Solution(_, _, never2) = t2.unify(t2_, s)
      Solution(Map(), Seq(), never1 ++ never2)
    case TUnifiable(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}
