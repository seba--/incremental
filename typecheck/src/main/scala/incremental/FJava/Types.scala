package incremental.FJava

import constraints.equality.CSubst
import constraints.equality.CSubst.CSubst
import constraints.CVar
import constraints.equality.{ConstraintSystem, EqConstraint, Type}
import incremental.{NodeKind, Node_}


/**
 * Created by lirakuci on 3/10/15.
 */

case object TNum extends Type {
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TNum => cs
    case UCName(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TString extends Type {
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TString => cs
    case UCName(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class CName(x: Symbol) extends Type {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = x == x2

  def subst(cs : CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (this == other) cs
    else cs.substitution.hget(CVar[Type](x)) match {
      case None => other match {
        case UCName(x) => other.unify(this, cs)
        case _ => cs.never(EqConstraint(this, other))
      }
      case Some(t) => t.unify(other, cs)
    }
  //other match {
    //case CName(x1) => other.unify(this,
      //if (x1 == x) cs.solved(CSubst(CVar[Type](x1) -> CName(x))) without(Set(CVar(x1)))// cs
      //else cs) // cs.never(EqConstraint(CName(x), CName(x1)))
    //case UCName(x1) => other.unify(this, cs.solved(CSubst(x1 -> CName(x))))
   // case _ => cs.never(EqConstraint(this, other))
 //}
}

case class UCName(x: CVar[Type]) extends Type { // should not use CVar, but CNAme, have to change that
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = x == x2

  def subst(s : CSubst) = s.hgetOrElse(x,this)

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

//other match {
//case UCName(x1) => other.unify(this, cs.solved(CSubst(x -> UCName(x1))))
//case CName(x1) => other.unify(this, cs.solved(CSubst(x -> CName(x1))))
//case _ => cs.never(EqConstraint(this, other))/
//}

case class Signature(ret: CName, m: Symbol, params: Map[Symbol, Type], bod : Type) extends Type{
  val returnTyp = this.ret
  val meth = this.m
  val parameters = this.params
  val body = this.bod
  def freeTVars = Set()
  def normalize = this
  def occurs(m2: CVar[_]) = m == m2
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case CName(x1) => other.unify(this,
      if (x1 == ret) cs.solved(CSubst(CVar[Type](x1) -> ret))
      else cs)
    case UCName(x1) => other.unify(this, cs.solved(CSubst(x1 -> ret)))
    case _ => cs.never(EqConstraint(this, other))
  }
}

