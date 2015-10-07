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
  def extend[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {
    case ProgramOK => cs
    case v@UCName(x) => v.supertype(this, cs)
    case _ => cs.never(Extend(this, other))
  }
}

case object TNum extends TBase(Set(Top))
case object TString extends TBase(Set(Top))


case class CName(x: Symbol) extends Type {
  val isGround = true

  def freeTVars = Set()

  def normalize = this

  def occurs(x2: CVar[_]) = x == x2

  def subst(cs: CSubst) = this

  def ||(that: Type) = Some(this)

  def &&(that: Type) = Some(that)

  def <(that: Type) = that == CName('Object)

  def subtype
  [CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS =
    other match {
      case CName(x2) =>
        if (x == x2) cs
        else if (findM(this, other, cs.extend)) cs
        else this.subtype(other, cs)
      case UCName(y) =>
        cs.substitution.hget(y) match {
          case Some(t) =>
            if (findM(t, other, cs.extend)) cs
            else t.subtype(other, cs)
          case None =>
            if (findM(this, other, cs.extend)) cs
            else this.subtype(other, cs)
        }
      case _ => cs.never(Subtype(this, other))
    }

  def findM(t1: Type, t2: Type, extend: Map[Type, Type]): Boolean
  = extend.get(t1) match {
    case None => false
    case Some(u) =>
      if (u == t2) true
      else findM(u, t2, extend)
  }

  def extend[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS =
    other match {
      case CName(x2) =>
        if (x == x2) cs
        else if (findM(this, other, cs.extend)) cs
        else this.extend(other, cs)
      //else  cs//this.subtype(other, cs)
      //   else cs.substitution.hget((CVar[Type](x))) match {
      //   case None => other match {
      //case UCName(x) => other.subtype(this, this.subtype(other,cs)) //have to check whetehr it should be subtype or supertype not clear
      case UCName(y) =>
        cs.substitution.hget(y) match {
          case Some(t) =>
            if (findM(t, other, cs.extend)) cs
            else t.extend(other, cs)
          case None =>
            if (findM(this, other, cs.extend)) cs
            else this.extend(other, cs)
        }
      case _ => cs.never(Extend(this, other))
    }
}

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
  def ||(that: Type) = Some(this)
  def &&(that: Type) = Some(that)
  def <(that: Type) = that == Top

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {
    case _ => cs.never(Subtype(this, other))
  }
  def extend[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {
    case _ => cs.never(Extend(this, other))
  }
}




