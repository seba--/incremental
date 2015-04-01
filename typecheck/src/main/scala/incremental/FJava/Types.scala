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
    case UVar(_) => other.unify(this, s)
 }
}
//
//case object Parameter extends Type {
//  def freeTVars = Set()
//  def occurs(x: Symbol) = false
//  def normalize = this
//def subst(s : TSubst) = this
//
//def unify(other: Type, s: TSubst) = other match {
//case UVar(_) => other.unify(this, s)
// }
//}
//
//case object Argument extends Type {
//  def freeTVars = Set()
//  def occurs(x: Symbol) = false
//  def normalize = this
//  def subst(s : TSubst) = this
//
//  def unify(other: Type, s: TSubst) = other match {
//    case UVar(_) => other.unify(this, s)
//  }
//}

case class UVar(x: Symbol) extends Type {
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def normalize = this
  def subst(s: TSubst) = s.getOrElse(x, this)
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


