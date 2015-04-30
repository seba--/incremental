package constraints.subtype.bounds

import constraints.subtype
import constraints.subtype.Constraint
import constraints.subtype.Type.Companion._

abstract class ConstraintSystemFactory[CS <: subtype.ConstraintSystem[CS]] extends subtype.ConstraintSystemFactory[CS] {
  val defaultBounds = Map[Symbol, (LBound, UBound)]().withDefaultValue((LBound(Set(), None), UBound(Set(), None)))

  def system(substitution: TSubst, bounds: Map[Symbol, (LBound, UBound)], never: Seq[Constraint]): CS
}
