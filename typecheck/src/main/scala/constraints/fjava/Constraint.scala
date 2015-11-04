package constraints.fjava

import constraints.fjava.CSubst.CSubst

trait Constraint {
  def subst(s: CSubst): Constraint

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
}

case class Extend(lower : Type, upper: Type) extends Constraint{
  def solve[CS <: ConstraintSystem[CS]](cs: CS) : CS = lower.extendz(upper, cs)

  override def subst(s: CSubst): Constraint = Extend(lower.subst(s), upper.subst(s))
}
case class Subtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = lower.subtype(upper, cs)

  override def subst(s: CSubst): Constraint = Subtype(lower.subst(s), upper.subst(s))
}

case class Equal(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = expected.unify(actual, cs)

  override def subst(s: CSubst): Constraint = Equal(expected.subst(s), actual.subst(s))
}

case class NotEqual(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.never(Equal(expected, actual))

  override def subst(s: CSubst): Constraint = NotEqual(expected.subst(s), actual.subst(s))
}

case class Never(c: Constraint) extends Constraint {
  def subst(s: CSubst) = c.subst(s)

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.never(this)
}

case class AllEqual(expected: List[Type], actual: List[Type]) extends Constraint {
  def subst(s: CSubst) = AllEqual(expected.map(_.subst(s)), actual.map(_.subst(s)))

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    var cons = Seq[Constraint]()
    for (i <- 0 until expected.size)
    cons = cons :+  Equal(expected(i), (actual(i)))

    cs.addNewConstraints(cons)
  }
}

