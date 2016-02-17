package constraints.fjava

import constraints.fjava.CSubst.CSubst
import incremental.fjava.Condition

trait Constraint {
  def subst(s: CSubst): Constraint

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
}

case class Subtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = lower.subtype(upper, cs)

  override def subst(s: CSubst): Constraint = Subtype(lower.subst(s), upper.subst(s))
}

case class NotSubtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = cs.never(this)

  override def subst(s: CSubst): Constraint = NotSubtype(lower.subst(s), upper.subst(s))
}

case class Equal(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = expected.unify(actual, cs)

  override def subst(s: CSubst): Constraint = Equal(expected.subst(s), actual.subst(s))
}

case class Conditional(cls: Type, cond: Condition, cons: Constraint) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    val cls_ = cls.subst(cs.substitution)
    cond.subst(cls_, cs.substitution) match {
      case None => cs // discard this constraint because condition is false
      case Some(cond_) if cond_.isGround => cons.solve(cs)
      case Some(cond_) => cs.notyet(Conditional(cls_, cond_, cons.subst(cs.substitution)))
    }
  }

  override def subst(s: CSubst): Constraint = {
    val cls_ = cls.subst(s)
    Conditional(cls_, cond.subst(cls_, s).getOrElse(Condition.trueCond), cons.subst(s))
  }
}

case class NotEqual(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.never(Equal(expected, actual))

  override def subst(s: CSubst): Constraint = NotEqual(expected.subst(s), actual.subst(s))
}

case class Never(c: Constraint) extends Constraint {
  def subst(s: CSubst) = c.subst(s)

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.never(this)
}

case class AllEqual(expected: Seq[Type], actual: Seq[Type]) extends Constraint {
  def subst(s: CSubst) = AllEqual(expected.map(_.subst(s)), actual.map(_.subst(s)))

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    if (expected.size != actual.size)
      cs.never(this)
    else {
      var cons = Seq[Constraint]()
      for (i <- 0 until expected.size)
        cons = cons :+ Equal(expected(i), actual(i))

      cs.addNewConstraints(cons)
    }
  }
}

case class StupidCastWarning(was: Type, castTo: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = cs.never(this)

  override def subst(s: CSubst): Constraint = StupidCastWarning(was.subst(s), castTo.subst(s))
}
