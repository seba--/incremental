package constraints.fjava

trait Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
}

case class Extend(lower : Type, upper: Type) extends Constraint{
  def solve[CS <: ConstraintSystem[CS]](cs: CS) : CS = lower.extendz(upper, cs)
}
case class Subtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = lower.subtype(upper, cs)
}

case class Equal(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = expected.subtype(actual, actual.subtype(expected, cs))
}