package constraints.javacons

import incremental.Java.syntax.Type /* TODO: new type trait like in other constraint systems
                                            or extend java type with unification functions? */
import constraints.javacons.CSubst.CSubst

/**
 * Created by qwert on 22.07.15.
 */
trait Constraint extends constraints.Constraint[Gen, Constraint]{

}

case class Equality(expected: Type, actual: Type) extends Constraint {
  def subst(s: CSubst) = Equality(expected.subst(s), actual.subst(s))
}

case class PrimitiveWidening(actual: Type, expected: Type) extends Constraint {
  def subst(s: CSubst) = PrimitiveWidening(actual.subst(s), expected.subst(s))
}

case class PrimitiveWideningString(actual: Type, expected: Type) extends Constraint {
  def subst(s: CSubst) = PrimitiveWideningString(actual.subst(s), expected.subst(s))
}

case class PrimitiveWideningEq(res: Type, actual: Type, expected: Type) extends Constraint {
  def subst(s: CSubst) = PrimitiveWideningEq(res.subst(s), actual.subst(s), expected.subst(s))
}

case class OneOf(actual: Type, expected: Seq[Type]) extends Constraint {
  def subst(s: CSubst) = SortOf(actual.subst(s), expected.map(_.subst(s)))
}