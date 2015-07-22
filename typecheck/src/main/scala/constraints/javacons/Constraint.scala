package constraints.javacons

import incremental.Java.syntax.Type /* TODO: new type trait like in other constraint systems
                                            or extend java type with unification functions? */
import constraints.javacons.CSubst.CSubst

/**
 * Created by qwert on 22.07.15.
 */
trait Constraint extends constraints.Constraint[Gen, Constraint]{

}

case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def subst(s: CSubst) = EqConstraint(expected.subst(s), actual.subst(s))
}