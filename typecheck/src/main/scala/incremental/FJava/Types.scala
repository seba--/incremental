package incremental.FJava

import incremental.{ClassT, EqConstraint,Type}
import incremental.ConstraintOps._
import incremental.Type.Companion.TSubst
/**
 * Created by lirakuci on 3/10/15.
 */
case class CName(x: Symbol) extends Type {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: Symbol) = x == x2

  def subst(s : TSubst) = this
  def unify(other: Type, s: TSubst) = other match {
    case CName(x1) => other.unify(this, s + (x1 -> CName(x)))
    case UCName(x1) => other.unify(this, s + (x1 -> CName(x)))
    case _ => never(EqConstraint(this, other))
 }
}
case class UCName(x: Symbol) extends Type {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: Symbol) = x == x2

  def subst(s : TSubst) = s.getOrElse(x,this)
  def unify(other: Type, s: TSubst) = other match {
  case UCName(x1) => other.unify(this, s + (x -> UCName(x1)))
  case CName(x1) => other.unify(this, s + (x -> CName(x1)))
  case _ => never(EqConstraint(this, other))
    }
}
//case class CName(x: Symbol) extends Type {
//
//  def freeTVars = Set()
//
//  def normalize = this
//
//  def occurs(x2: Symbol) = x == x2
//
//  def subst(s: TSubst) = this
//
//  def unify(other: Type, s: TSubst) = other match {
//    case CName(x1) => other.unify(this, s + (x1 -> CName(x)))
//    case UCName(x1) => other.unify(this, s + (x1 -> CName(x)))
//    case _ => never(EqConstraint(this, other))
//  }
//}
//
//case class UCName(x: Symbol) extends Type {
//  def freeTVars = Set()
//
//  def occurs(x2: Symbol) = x == x2
//
//  def normalize = this
//
//  def subst(s: Map[Symbol, Type]) = s.getOrElse(x, this)
//
//  def unify(other: Type, s: TSubst) =
//    if (other == this) emptySol
//    else s.get(x) match {
//      case Some(t) => t.unify(other, s)
//      case None =>
//        val t = other.subst(s)
//        if (this == t)
//          emptySol
//        else if (t.occurs(x))
//          never(EqConstraint(this, t))
//        else
//          solution(Map(x -> t))
//    }
//}