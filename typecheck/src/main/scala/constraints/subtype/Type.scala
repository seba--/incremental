package constraints.subtype

import constraints.TypeCompanion

//Type class for types with groundness test
trait Type extends constraints.Type {
  def occurs(x: Symbol): Boolean
  def subst(s: Type.Companion.TSubst): Type
  val isGround: Boolean

  def ||(that: Type): Option[Type]
  def &&(that: Type): Option[Type]
  def <(that: Type): Boolean

  def subtype[CS <: ConstraintSystem[CS]]
    (other: Type, s: Type.Companion.TSubst)
    (implicit csf: ConstraintSystemFactory[CS])
    : CS

  def subtype[CS <: ConstraintSystem[CS]]
    (other: Type)
    (implicit csf: ConstraintSystemFactory[CS]): CS
  = subtype(other, Map())
}

object Type {
  implicit object Companion extends TypeCompanion {
    type TError = String
    type TSubst = Map[Symbol, Type]
  }
}
import Type.Companion._

case class UVar(x: Symbol) extends Type {
  val isGround = false
  def occurs(x2: Symbol) = x == x2
  def subst(s: TSubst): Type = s.getOrElse(x, this)

  def ||(that:Type) = that match {
    case Top => Some(Top)
    case _ => None // was Some(Top)
  }

  def &&(that: Type) = that match {
    case Top => Some(this)
    case _ => None
  }

  def <(that: Type) = that == Top

  def subtype
    [CS <: ConstraintSystem[CS]]
    (other: Type, s: Type.Companion.TSubst)
    (implicit csf: ConstraintSystemFactory[CS]): CS
  = {
    val gen = csf.state.value.gen.asInstanceOf[Gen]
    other match {
      case UVar(`x`) => csf.emptySolution
      case UVar(y) =>
        if (gen.isNegative(x))
          csf.emptySolution.addUpperBound(x, other)
        else {
          val cs = csf.emptySolution.addUpperBound(x, other)
          cs.addLowerBound(y, this)
        }
      case _ =>
        if (other.occurs(x))
          csf.never(Subtype(this, other))
        else
          csf.emptySolution.addUpperBound(x, other)
    }
  }

  def supertype
    [CS <: ConstraintSystem[CS]]
    (other: Type, s: Type.Companion.TSubst)
    (implicit csf: ConstraintSystemFactory[CS]): CS
  = {
    other match {
      case UVar(`x`) => csf.emptySolution
      case UVar(y) => other.subtype(this)
      case _ =>
        if (other.occurs(x))
          csf.never(Subtype(other, this))
        else
          csf.emptySolution.addLowerBound(x, other)
    }
  }
}

case object Top extends Type {
  val isGround = true
  def occurs(x: Symbol) = false
  def subst(s: TSubst) = this

  def ||(that: Type) = Some(this)
  def &&(that: Type) = Some(that)
  def <(that: Type) = that == Top

  def subtype
    [CS <: ConstraintSystem[CS]]
    (other: Type, s: Type.Companion.TSubst)
    (implicit csf: ConstraintSystemFactory[CS]): CS
  = other match {
    case Top => csf.emptySolution
    case v@UVar(x) => v.supertype(this, s)
    case _ => csf.never(Subtype(this, other))
  }


//  private[ConstraintSystem] def normalizeSub(s: Type, t: Type): ConstraintSystem = (s,t) match {
//    case (t1, t2) if t1 == t2 =>
//    case (_, Top) =>

//    case (UVar(a), UVar(b)) =>
//      if (isNegative(a))
//        addUpperBound(a, t)
//      else {
//        addUpperBound(a, t)
//        addLowerBound(b, s)
//      }
//
//    case (UVar(a), t2) =>
//      if (t2.occurs(a))
//        gameOver(Subtype(s, t))
//      else
//        addUpperBound(a, t2)

//    case (t1, UVar(a)) =>
//      if (t1.occurs(a))
//        gameOver(Subtype(s, t))
//      else
//        addLowerBound(a, t1)

//    case (TNum, TNumeric) =>
//    case (TFloat, TNumeric) =>
//    case (s1 -->: t1, s2 -->: t2) =>
//      normalizeSub(s2, s1)
//      normalizeSub(t1, t2)
//    case _ =>
//      gameOver(Subtype(s, t))
//  }
}

