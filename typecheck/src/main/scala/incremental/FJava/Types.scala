package incremental.FJava

import constraints.equality.CSubst
import constraints.equality.CSubst.CSubst
import constraints.CVar
import constraints.equality.{ConstraintSystem, ConstraintSystemFactory, EqConstraint, Type}


/**
 * Created by lirakuci on 3/10/15.
 */
case class CName(x: Symbol) extends Type {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = x == x2

  def subst(cs : CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case CName(x1) => other.unify(this, cs.solved(CSubst(CVar[Type](x1) -> CName(x))))
    case UCName(x1) => other.unify(this, cs.solved(CSubst(x1 -> CName(x))))
    case _ => cs.never(EqConstraint(this, other))
 }
}
case class UCName(x: CVar[Type]) extends Type { // should not use CVar, but CNAme, have to change that
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = x == x2

  def subst(s : CSubst) = s.hgetOrElse(x,this)
  //def unify(other: Type, s: TSubst) = other match {
  //case UCName(x1) => other.unify(this, s + (x -> UCName(x1)))
  //case CName(x1) => other.unify(this, s + (x -> CName(x1)))
  //case _ => never(EqConstraint(this, other))
    //}
//}
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