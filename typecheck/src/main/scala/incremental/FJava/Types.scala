package incremental.fjava

import constraints.CVar
import constraints.fjava.CSubst
import constraints.fjava.CSubst._
import constraints.fjava._

import scala.collection.immutable.ListMap

/**
 * Created by lirakuci on 3/10/15.
 */


case class UCName(x: CVar[Type]) extends Type {
  val isGround = false
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst): Type = s.hgetOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (other == this) cs
    else cs.substitution.hget(x) match {
      case Some(t) => t.unify(other, cs)
      case None =>
        val t: Type = other.subst(cs.substitution)
        if (this == t)
          cs
        else if (t.occurs(x))
          cs.never(Equal(this, t))
        else
          cs.solved(CSubst(x -> t))
    }

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS =
    if (this == other)  cs
    else cs.substitution.hget(x) match {
      case Some(t) =>
        t.subtype(other, cs)
      case None =>
        cs.addUpperBound(this, other)
  }

  def extendz[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.extendz(this, other)
}

case class CName(x: Symbol) extends Type {
  val isGround = true
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = x == x2
  def subst(cs: CSubst) = this
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.addUpperBound(this, other)
  def extendz[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.extendz(this, other)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other match {
    case CName(`x`) => cs
    case UCName(_) => other.unify(this, cs)
    case _ => cs.never(Equal(this, other))
  }
}

case object ProgramOK extends  Type
{
  val isGround = true
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = false
  def subst(cs: CSubst) = this
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = ???
  def extendz[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = ???
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = if (other == this) cs else cs.never(Equal(this, other))
}

case object ClassOK extends  Type
{
  val isGround = true
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = false
  def subst(cs: CSubst) = this
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.addUpperBound(this, other)
  def extendz[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.extendz(this, other)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = this.unify(other, cs)
}

case object MethodOK extends  Type
{
  val isGround = true
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = false
  def subst(cs: CSubst) = this
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.addUpperBound(this, other)
  def extendz[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.extendz(this, other)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other.unify(this, cs)
}
