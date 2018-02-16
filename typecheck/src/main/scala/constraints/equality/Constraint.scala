package constraints.equality

import constraints.equality.CSubst.CSubst
import incremental.typeclasses.{TUniv, TVar, UUniv}

trait Constraint extends constraints.Constraint[Gen, Constraint] {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS
}

case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = expected.unify(actual, cs)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
  def subst(s: CSubst) = EqConstraint(expected.subst(s), actual.subst(s))
}

case class NotUniv(t1 : Type, t2 : Type) extends Constraint {
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
  def subst(s: CSubst) = NotUniv(t1.subst(s), t2.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = (t1, t2) match {
    case (TUniv(_, _), _) => cs.never(EqConstraint(t1,t2))
    case (UUniv(_, _), _ ) => cs.never(EqConstraint(t1,t2))
    case (TVar(_), _) => cs.never(EqConstraint(t1,t2))
    case _ => cs//.notyet(EqConstraint(t1,t2))


    //case (TVar(_)) => cs.notyet(this)
  }
}
