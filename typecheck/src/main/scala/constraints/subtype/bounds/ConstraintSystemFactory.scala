package constraints.subtype.bounds

import constraints.subtype.Constraint
import constraints.subtype.Type.Companion.TSubst

object ConstraintSystemFactory extends constraints.subtype.ConstraintSystemFactory[ConstraintSystem] {
  val defaultBounds = Map[Symbol, (LBound, UBound)]().withDefaultValue((LBound(Set(), None), UBound(Set(), None)))
  def freshConstraintSystem = ConstraintSystem(Map(), defaultBounds, Seq())
  def solved(s: TSubst) = ConstraintSystem(s, defaultBounds, Seq())
  def notyet(c: Constraint) = freshConstraintSystem addNewConstraint (c)
  def never(c: Constraint) = ConstraintSystem(Map(), defaultBounds, Seq(c))
}
