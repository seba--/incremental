package incremental.fjavaGen

import constraints.CVar
import constraints.fjavaGen.CSubst._
import constraints.fjavaGen.{CSubst, _}

/**
 * Created by lirakuci on 3/10/15.
 */

trait GenType extends Type {
  def freeTVars: Set[Symbol]
  def getFreeTVars(t: Type): Set[Symbol] =
    if (t.isInstanceOf[GenType])
      t.asInstanceOf[GenType].freeTVars
    else
      Set()
}

case class TVar(alpha : Symbol) extends GenType {
  def isGround = false
  def uvars: Set[CVar[Type]] = Set()
  def freeTVars = Set(alpha)
  def occurs(x2: CVar[_]) = alpha == x2
  def subst(s: CSubst) = s.hgetOrElse(CVar[Type](alpha), this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) =
    if (this == other) cs
    else cs.substitution.hget(CVar[Type](alpha)) match {
      case None => other match {
        case UCName(x) => other.unify(this, cs)
        case _ => cs.never(Equal(this, other))
      }
      case Some(t) => t.unify(other, cs)
    }
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS =
    if (this == other)
      cs
    else
      cs.addUpperBound(this.subst(cs.substitution), other.subst(cs.substitution))

}


case class UCName(x: CVar[Type]) extends GenType {
  def isGround = false
  def freeTVars: Set[Symbol] = Set()
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
    if (this == other)
      cs
    else
      cs.addUpperBound(this.subst(cs.substitution), other.subst(cs.substitution))


  def uvars = Set(x)

  override val hashCode: Int = x.x.hashCode()
}

case object TNum extends GroundType {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = false
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = ???
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = if (other == this) cs else cs.never(Equal(this, other))
  def uvars = Set()
}


case class CName(x: Symbol, params: Seq[Type]) extends GroundType {
//  def isGround = {
//    var flg = true
//    for (i<- 0 until params.length) {
//      if (!params(i).isGround)
//        flg = false
//    }
//    flg
//  }
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = x == x2
 // def subst(s: CSubst): Type = CName(x, params.map(p => p.subst(s)))
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.addUpperBound(this, other.subst(cs.substitution))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other match {
    case CName(`x`, paramsN) => cs
    case UCName(_) => other.unify(this, cs)
    case _ => cs.never(Equal(this, other))
  }

  override def toString: String = x.name
  def uvars = Set()

  override val hashCode: Int = x.hashCode()
}
case object CName {
  def apply(s: String, params : Seq[Type]): CName = CName(Symbol(s), params)
}

case object ProgramOK extends GroundType {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = false
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = ???
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = if (other == this) cs else cs.never(Equal(this, other))
  def uvars = Set()
}

case object ClassOK extends GroundType {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = false
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.addUpperBound(this, other)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = this.unify(other, cs)
  def uvars = Set()
}

case object MethodOK extends GroundType {
  def freeTVars = Set()
  def normalize = this
  def occurs(x2: CVar[_]) = false
  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = cs.addUpperBound(this, other)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other.unify(this, cs)
  def uvars = Set()
}
