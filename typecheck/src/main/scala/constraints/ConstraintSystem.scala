package constraints

import CSubst.CSubst
import scala.collection.generic.CanBuildFrom

trait ConstraintSystem[G <: GenBase, C, CS] {
  def state: State[G]
  def gen: G = state.gen

  def substitution: CSubst[C]
  def isSolved: Boolean
  def mergeSubsystem(that: CS): CS
  def addNewConstraint (that: C): CS
  def addNewConstraints(cons: Iterable[C]): CS
  def applyPartialSolution[CT <: CTerm[G, C, CT]](t: CT): CT
  def applyPartialSolutionIt[U, Col <: Iterable[U], CT <: CTerm[G, C, CT]]
    (it: Col, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), Col]): Col
  def shouldApplySubst: Boolean
  def propagate: CS
  def tryFinalize: ConstraintSystem[G, C, _]
}
