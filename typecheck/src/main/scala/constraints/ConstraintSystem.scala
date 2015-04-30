package constraints

import incremental.MyBuilder

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

trait ConstraintSystem[CS, Constraint, T <: Type] {
  def isSolved: Boolean
  def mergeSubsystem(that: CS): CS
  def addNewConstraint (that: Constraint): CS
  def addNewConstraints(cs: Iterable[Constraint]): CS
  def applyPartialSolution(t: T): T
  def applyPartialSolutionIt[U, C <: Iterable[U]](it: C, f: U=>T)
                                                 (implicit bf: CanBuildFrom[Iterable[U], (U, T), C]): C
  def propagate: CS
  def tryFinalize: ConstraintSystem[_,Constraint,T]
}
