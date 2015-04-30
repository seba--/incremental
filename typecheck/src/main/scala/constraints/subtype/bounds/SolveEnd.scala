package constraints.subtype.bounds

import constraints.subtype.Constraint
import constraints.subtype.Type.Companion._

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = new SolveEndCS(Map(), defaultBounds, Seq())
  def solved(s: TSubst) = new SolveEndCS(s, defaultBounds, Seq())
  def notyet(c: Constraint) = freshConstraintSystem addNewConstraint (c)
  def never(c: Constraint) = new SolveEndCS(Map(), defaultBounds, Seq(c))
  def system(substitution: TSubst, bounds: Map[Symbol, (LBound, UBound)], never: Seq[Constraint]) = new SolveEndCS(substitution, bounds, never)
}

class SolveEndCS(substitution: TSubst, bounds: Map[Symbol, (LBound, UBound)], never: Seq[Constraint]) extends ConstraintSystem[SolveEndCS](substitution, bounds, never) {
  val csf = SolveEnd
}