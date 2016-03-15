package constraints.javacons

import incremental.java.syntax.Type /* TODO: new type trait like in other constraint systems
                                            or extend java type with unification functions? */
import constraints.javacons.CSubst.CSubst

/**
 * Created by qwert on 22.07.15.
 */
trait Constraint extends constraints.Constraint[Gen, Constraint]{
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS
}

case class Equality(expected: Type, actual: Type) extends Constraint {
  def subst(s: CSubst) = Equality(expected.subst(s), actual.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = expected.unify(actual, cs)
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
}

case class PrimitiveWidening(actual: Type, expected: Type) extends Constraint {
  def subst(s: CSubst) = PrimitiveWidening(actual.subst(s), expected.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
}

case class PrimitiveWideningString(actual: Type, expected: Type) extends Constraint {
  def subst(s: CSubst) = PrimitiveWideningString(actual.subst(s), expected.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
}

case class PrimitiveWideningEq(res: Type, actual: Type, expected: Type) extends Constraint {
  def subst(s: CSubst) = PrimitiveWideningEq(res.subst(s), actual.subst(s), expected.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
}

case class DirectedWidening(lower: Type, upper: Type) extends Constraint {
  def subst(s: CSubst) = DirectedWidening(lower.subst(s), upper.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
}

case class OneOf(actual: Type, expected: Seq[Type]) extends Constraint {
  def subst(s: CSubst) = OneOf(actual.subst(s), expected.map(_.subst(s)))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
}

case class Subtype(lower: Type, upper: Type) extends Constraint {
  def subst(s: CSubst) = Subtype(lower.subst(s), upper.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
}

case class SubtypeOrDirectedWidening(lower: Type, upper: Type) extends Constraint {
  def subst(s: CSubst) = SubtypeOrDirectedWidening(lower.subst(s), upper.subst(s))
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = ???
}