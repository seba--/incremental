package constraints.subtype.impl

import constraints.subtype
import constraints.subtype.Constraint
import constraints.subtype.Type.Companion._

abstract class ConstraintSystemFactory[CS <: subtype.ConstraintSystem[CS]] extends subtype.ConstraintSystemFactory[CS] {
  val defaultBounds = Map[Symbol, (LBound, UBound)]().withDefaultValue((LBound(Set(), None), UBound(Set(), None)))
}
