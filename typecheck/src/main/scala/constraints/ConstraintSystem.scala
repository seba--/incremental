package constraints

trait ConstraintSystem[CS, Constraint, T <: Type] {
  def isSolved: Boolean
//  def solution: Solution
  def mergeSubsystem(that: CS): CS
  def addNewConstraint (that: Constraint): CS
  def addNewConstraints(cs: Iterable[Constraint]): CS
  def applyPartialSolution(t: T): T
  def propagate: CS
  def tryFinalize: CS
}
