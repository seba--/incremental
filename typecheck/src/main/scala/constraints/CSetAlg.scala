package constraints

trait CSetAlg[CS, Constraint, Solution] {
  def isSolved: Boolean
  def solution: Solution
  def ++(that: CS): CS
  def + (that: Constraint): CS
  def ++(cs: Iterable[Constraint]): CS
  def tryFinalize: CS
}
