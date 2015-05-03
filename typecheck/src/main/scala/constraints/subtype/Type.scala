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

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
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

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = if (this == other)  cs
    else cs.substitution.get(x) match {
      case Some(t) => t.subtype(other, cs)
      case None => other match {
        case UVar(y) =>
          if (cs.state.gen.isNegative(x))
            cs.addUpperBound(x, other)
          else
            cs.addUpperBound(x, other).addLowerBound(y, this)
        case _ =>
          if (other.occurs(x))
            cs.never(Subtype(this, other))
          else
            cs.addUpperBound(x, other)
      }
    }


  def supertype
    [CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = if (this == other)  cs
    else cs.substitution.get(x) match {
      case Some(t) => other.subtype(t, cs)
      case None => other match {
        case UVar(y) => other.subtype(this, cs)
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
  def occurs(x: Symbol) = false
  def subst(s: TSubst) = this

  def ||(that: Type) = Some(this)
  def &&(that: Type) = Some(that)
  def <(that: Type) = that == Top

  def subtype
    [CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  = other match {
    case Top => cs
    case v@UVar(x) => v.supertype(this, cs)
    case _ => cs.never(Subtype(this, other))
  }

}

