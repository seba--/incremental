package incremental.systemfomega

import constraints.normequality.CSubst.CSubst
import constraints.{CTermBase, CVar}
import constraints.normequality.CSubst.CSubst
import constraints.normequality.{CSubst, CTerm, ConstraintSystem, Constraint}

trait Kind extends CTerm[Kind] {
  def occurs(x: CVar[_]): Boolean
  def unify[CS <: ConstraintSystem[CS]](other: Kind, cs: CS): CS

  def compatibleWith(t2: Kind) = EqKindConstraint(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = EqKindConstraint(this, t2.asInstanceOf[Kind])
}

case class KUvar(x: CVar[Kind]) extends Kind {
  def subst(s: CSubst) = s.hgetOrElse(x, this)
  def occurs(x2: CVar[_]) = x == x2
  def unify[CS <: ConstraintSystem[CS]](other: Kind, cs: CS) =
    if (other == this) cs
    else cs.substitution.hget(x) match {
      case Some(t) => t.unify(other, cs)
      case None =>
        val k = other.subst(cs.substitution)
        if (this == k)
          cs
        else if (k.occurs(x))
          cs.never(EqKindConstraint(this, k))
        else
          cs.solved(CSubst(x -> k))
    }
}

case object KStar extends Kind {
  def subst(s: CSubst) = this
  def occurs(x: CVar[_]) = false
  def unify[CS <: ConstraintSystem[CS]](other: Kind, cs: CS) = other match {
    case KStar => cs
    case KUvar(_) => other.unify(this, cs)
    case _ => cs.never(EqKindConstraint(this, other))
  }
}

case class KArrow(k1: Kind, k2: Kind) extends Kind {
  def subst(s: CSubst) = KArrow(k1.subst(s), k2.subst(s))
  def occurs(x: CVar[_]) = k1.occurs(x) || k2.occurs(x)
  def unify[CS <: ConstraintSystem[CS]](other: Kind, cs: CS) = other match {
    case KArrow(k21, k22) => k1.unify(k21, k2.unify(k22, cs))
    case KUvar(_) => other.unify(this, cs)
    case _ => cs.never(EqKindConstraint(this, other))
  }
}


case class EqKindConstraint(k1: Kind, k2: Kind) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = k1.unify(k2, cs)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)
  def subst(s: CSubst) = EqKindConstraint(k1.subst(s), k2.subst(s))
}