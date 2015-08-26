package constraints.subtype

trait Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
}

case class Subtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = lower.subtype(upper, cs)
}

case class Equal(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = expected.subtype(actual, actual.subtype(expected, cs))
}

case class Join(target: Type, ts: Set[Type]) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = ts.foldLeft(cs)((cs, t) => t.subtype(target, cs))
}

case class Meet(target: Type, ts: Set[Type]) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = ts.foldLeft(cs)((cs, t) => target.subtype(t, cs))
}

case class NotEqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS =  cs.never(Subtype(expected, actual))  // actual.unify(actual, cs)

}
