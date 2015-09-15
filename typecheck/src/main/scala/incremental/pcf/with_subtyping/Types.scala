package incremental.pcf.with_subtyping

import constraints.CVar
import constraints.subtype.CSubst.CSubst
import constraints.subtype._


/**
 * Created by oliver on 19.11.14.
 */

class TBase(val directSupertypes: Set[Type]) extends Type {
  val supertypes: Set[Type] =
    directSupertypes ++ directSupertypes.flatMap(t =>
      if (t.isInstanceOf[TBase])
        t.asInstanceOf[TBase].supertypes
      else
        Set[Type]()
    )

  val isGround = true
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this

  def ||(that: Type) =
    if (this == that)
      Some(this)
    else if (supertypes.contains(that))
      Some(that)
    else if (that.isInstanceOf[TBase] && that.asInstanceOf[TBase].supertypes.contains(this))
      Some(this)
    else
      Some(Top)

  def &&(that: Type) =
    if (this == that)
      Some(this)
    else if (supertypes.contains(that))
      Some(this)
    else if (that.isInstanceOf[TBase] && that.asInstanceOf[TBase].supertypes.contains(this))
      Some(that)
    else
      None

  def <(that: Type) =
    if (this == that)
      true
    else if (supertypes.contains(that))
      true
    else
      false

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (this == other || supertypes.contains(other))
      cs
    else other match {
      case v@UCName(_) => v.supertype(this, cs)
      case _ => cs.never(Subtype(this, other))
    }
}

case object TNumeric extends TBase(Set(Top))
case object TInteger extends TBase(Set(TNumeric))
case object TFloat extends TBase(Set(TNumeric))


case class TFun(t1: Type, t2: Type) extends Type {
  val isGround = t1.isGround && t2.isGround
  def occurs(x: CVar[_]) = t1.occurs(x) || t2.occurs(x)
  def subst(s: CSubst) = TFun(t1.subst(s), t2.subst(s))

  def ||(that: Type) = that match {
    case TFun(u1, u2) => (t1 && u1, t2 || u2) match {
      case (Some(arg), Some(res)) => Some(TFun(arg, res))
      case _ => Some(Top)
    }
    case _ => Some(Top)
  }

  def &&(that: Type) = that match {
    case Top => Some(this)
    case TFun(u1, u2) => (t1 || u1, t2 && u2) match {
      case (Some(arg), Some(res)) => Some(TFun(arg, res))
      case _ => None
    }
    case _ => None
  }

  def <(that: Type) = that match {
    case Top => true
    case TFun(u1, u2) => u1 < t1 && t2 < u2
    case _ => false
  }

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case Top => cs
    case TFun(u1, u2) => t2.subtype(u2, u1.subtype(t1, cs))
    case v@UCName(_) => v.supertype(this, cs)
    case _ => cs.never(Subtype(this, other))
  }
}
