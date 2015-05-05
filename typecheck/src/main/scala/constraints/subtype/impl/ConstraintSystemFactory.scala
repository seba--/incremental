package constraints.subtype.impl

import constraints.subtype.Type
import constraints.{CVar, subtype}

abstract class ConstraintSystemFactory[CS <: subtype.ConstraintSystem[CS]] extends subtype.ConstraintSystemFactory[CS] {
  val defaultBounds = Map[CVar[Type], (LBound, UBound)]().withDefaultValue((LBound(Set(), None), UBound(Set(), None)))
}
