package incremental.FJava

import constraints.subtype.CSubst.CSubst
import constraints.CVar
import constraints.subtype._
import incremental.pcf.with_subtyping.TBase
import incremental.{NodeKind, Node_}


/**
 * Created by lirakuci on 3/10/15.
 */

case object ProgramOK extends Type {
  val isGround = true
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this

  def ||(that: Type) = Some(this)
  def &&(that: Type) = Some(that)
  def <(that: Type) = that == Top

  def subtype
  [CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {
    case ProgramOK => cs
    case v@UCName(x) => v.supertype(this, cs)
    case _ => cs.never(Subtype(this, other))
  }
}


case object TNum extends TBase(Set(Top))

case object TString extends TBase(Set(Top))

case object OObject extends TBase(Set(Top))
case object Title extends TBase(Set(OObject))
case object NoTitle extends TBase(Set(OObject))
case object ProfTitle extends TBase(Set(Title))
case object Person extends TBase(Set(OObject))
case object Porfessor extends TBase(Set(Person))
case object Student extends TBase(Set(Person))

case class CName(x: Symbol) extends Type{
  val isGround = true

  def freeTVars = Set()

  def normalize = this

  def occurs(x2: CVar[_]) = x == x2

  def subst(cs: CSubst) = this

  //other match {
  //case CName(x1) => other.unify(this,
  //if (x1 == x) cs.solved(CSubst(CVar[Type](x1) -> CName(x))) without(Set(CVar(x1)))// cs
  //else cs) // cs.never(EqConstraint(CName(x), CName(x1)))
  //case UCName(x1) => other.unify(this, cs.solved(CSubst(x1 -> CName(x))))
  // case _ => cs.never(EqConstraint(this, other))
  //}

  def ||(that: Type) = Some(this)

  def &&(that: Type) = Some(that)

  def <(that: Type) = that == CName('Object)

  def subtype
  [CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS =
    other match {
      case CName(x2) =>
        if (x == x2) cs
        else  cs//this.subtype(other, cs)
 //   else cs.substitution.hget((CVar[Type](x))) match {
   //   case None => other match {
        //case UCName(x) => other.subtype(this, this.subtype(other,cs)) //have to check whetehr it should be subtype or supertype not clear
      case v@UCName(_) => v.supertype(other,cs)
      case _ => cs.never(Subtype(this, other))
      }
   //   case Some(t) => t.subtype(other, other.subtype(t,cs))
   // }

}

//case class UCName(x: CVar[Type]) extends Type { // should not use CVar, but CNAme, have to change that
//val isGround = false
//  def freeTVars = Set()
//  def normalize = this
//  def occurs(x2: CVar[_]) = x == x2
//
//  def subst(s : CSubst) = s.hgetOrElse(x,this)
//
//
//  def supertype
//  [CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
//  = if (this == other)  cs
//  else cs.substitution.hget(x) match {
//    case Some(t) => other.subtype(t, cs)
//    case None => other match {
//      case UCName(y) => other.subtype(this, cs)
//      case _ =>
//        if (other.occurs(x))
//          cs.never(Subtype(other, this))
//        else
//          cs.addLowerBound(x, other)
//    }
//  }
//  def ||(that: Type) = Some(this)
//  def &&(that: Type) = Some(that)
//  def <(that: Type) = that == Top
//
//  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
//  = if (this == other)  cs
//  else cs.substitution.hget(x) match {
//    case Some(t) => t.subtype(other, cs)
//    case None => other match {
//      case UCName(y) =>
//        if (cs.state.gen.isNegative(x))
//          cs.addUpperBound(x, other)
//        else
//          cs.addUpperBound(x, other).addLowerBound(y, this)
//      case _ =>
//        if (other.occurs(x))
//          cs.never(Subtype(this, other))
//        else
//          cs.addUpperBound(x, other)
//    }
//  }
//}



//other match {
//case UCName(x1) => other.unify(this, cs.solved(CSubst(x -> UCName(x1))))
//case CName(x1) => other.unify(this, cs.solved(CSubst(x -> CName(x1))))
//case _ => cs.never(EqConstraint(this, other))/
//}

case class Signature(ret: CName, m: Symbol, params: Map[Symbol, Type], bod : Type) extends Type{
  val isGround = true
  val returnTyp = this.ret
  val meth = this.m
  val parameters = this.params
  val body = this.bod
  def freeTVars = Set()
  def normalize = this
  def occurs(m2: CVar[_]) = m == m2
  def subst(s: CSubst) = this
//  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
   // case CName(x1) => other.subtype(this,x1)
   // case UCName(x1) => other.subtype(this, x1)
  //  case _ => cs.never(Subtype(this, other))
 // }
  def ||(that: Type) = Some(this)
  def &&(that: Type) = Some(that)
  def <(that: Type) = that == Top

  def subtype
  [CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {

    case _ => cs.never(Subtype(this, other))
  }
}




