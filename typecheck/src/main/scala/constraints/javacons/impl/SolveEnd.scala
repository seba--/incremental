package constraints.javacons.impl

import constraints.javacons.CSubst.CSubst
import constraints.{State, CTerm}
import constraints.javacons._

import scala.collection.generic.CanBuildFrom

/**
 * Created by qwert on 26.01.16.
 */

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCS(Seq())
}

case class SolveEndCS(notyet: Seq[Constraint]) extends ConstraintSystem[SolveEndCS] {
  def state: State[Gen] = SolveEnd.state.value

  def substitution: CSubst = CSubst.empty

  def tryFinalize: ConstraintSystem[_] = ???

  def isSolved: Boolean = ???

  def applyPartialSolutionIt[U, Col <: Iterable[U], CT <: CTerm[Gen, Constraint, CT]](it: Col, f: (U) => CT)(implicit bf: CanBuildFrom[Iterable[U], (U, CT), Col]): Col = ???

  def propagate: SolveEndCS = ???

  def addNewConstraint(that: Constraint): SolveEndCS = ???

  def applyPartialSolution[CT <: CTerm[Gen, Constraint, CT]](t: CT): CT = ???

  def mergeSubsystem(that: SolveEndCS): SolveEndCS = ???

  def addNewConstraints(cons: Iterable[Constraint]): SolveEndCS = ???
}
