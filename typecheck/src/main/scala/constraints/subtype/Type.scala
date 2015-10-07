package constraints.subtype

import constraints.{CTermBase, CVar}
import constraints.subtype.CSubst.CSubst
import constraints.subtype._
//Type class for types with groundness test
trait Type extends CTerm[Type] {
  def occurs(x: CVar[_]): Boolean
  def subst(s: CSubst): Type
  val isGround: Boolean

  def ||(that: Type): Option[Type]
  def &&(that: Type): Option[Type]
  def <(that: Type): Boolean

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def extend[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def compatibleWith(t2: Type) = Equal(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = Equal(this, t2.asInstanceOf[Type])

}

case class UCName(x: CVar[Type]) extends Type {
  val isGround = false
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst): Type = s.hgetOrElse(x, this)

  def ||(that:Type) = that match {
    case Top => Some(Top)
    case _ => None // was Some(Top)
  }

  def &&(that: Type) = that match {
    case Top => Some(this)
    case _ => None
  }

  def <(that: Type) = that == Top

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
   = if (this == other)  cs
    else cs.substitution.hget(x) match {
      case Some(t) =>
        if (findM(t, other, cs.extend)) cs
        else t.subtype(other, cs)
      case None => other match {
        case UCName(y) =>
          if (findM(this, other, cs.extend)) cs
          else this.subtype(other, cs)
        case _ =>
          if (other.occurs(x))
            cs.never(Subtype(this, other))
          else
            cs.addUpperBound(x, other)
      }
    }

  def findM(t1 : Type, t2 : Type, extend: Map[Type, Type]): Boolean
  = extend.get(t1) match {
    case None => false
    case Some(u) =>
      if (u == t2) true
      else findM(u, t2, extend)
  }


  def extend[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = if (this == other)  cs
  else cs.substitution.hget(x) match {
    case Some(t) =>  t.extend(other, cs)
    case None => other match {
      case UCName(y) =>
      cs.addExtend(this, other) //UCName(x), UCName (y)
      case _ => cs.never(Extend(other, this))
    }
  }

  def supertype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = if (this == other)  cs
    else cs.substitution.hget(x) match {
      case Some(t) => other.subtype(t, cs)
      case None => other match {
        case UCName(y) => other.subtype(this, cs)
        case _ =>
          if (other.occurs(x))
            cs.never(Subtype(other, this))
          else
            cs.addLowerBound(x, other)
      }
    }
}

case object Top extends Type {
  val isGround = true
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this

  def ||(that: Type) = Some(this)
  def &&(that: Type) = Some(that)
  def <(that: Type) = that == Top

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {
    case Top => cs
    case v@UCName(x) => v.supertype(this, cs)
    case _ => cs.never(Subtype(this, other))
  }
  def extend[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {
    case Top => cs
    case v@UCName(x) => v.supertype(this, cs)
    case _ => cs.never(Extend(this, other))
  }

}

