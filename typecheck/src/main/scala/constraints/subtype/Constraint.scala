package constraints.subtype

import constraints.subtype.Type.Companion.TSubst

trait Constraint {
  def solve[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS])(implicit csf: ConstraintSystemFactory[CS]): CS
}

case class Subtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS])(implicit csf: ConstraintSystemFactory[CS]): CS =
    lower.subtype(upper, s.substitution)
}

case class Equal(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS])(implicit csf: ConstraintSystemFactory[CS]) = {
    val cs1 = expected.subtype(actual, s.substitution)
    val cs2 = actual.subtype(expected, s.substitution)
    cs1 mergeSubsystem cs2
  }
}

case class Join(target: Type, ts: Set[Type]) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS])(implicit csf: ConstraintSystemFactory[CS]) = {
    val subst = s.substitution
    ts.foldLeft(csf.freshConstraintSystem)((cs, t) => cs mergeSubsystem t.subtype(target, subst))
  }
}

case class Meet(target: Type, ts: Set[Type]) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS])(implicit csf: ConstraintSystemFactory[CS]) = {
    val subst = s.substitution
    ts.foldLeft(csf.freshConstraintSystem)((cs, t) => cs mergeSubsystem target.subtype(t, subst))
  }
}
