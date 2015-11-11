package constraints.javacons.impl

import constraints.Statistics
import constraints.javacons.CSubst.CSubst
import constraints.javacons._
import incremental.Util

import scala.collection.generic.CanBuildFrom

/**
 * Created by qwert on 11.11.15.
 */

object JustCollect extends ConstraintSystemFactory[JustCollectCS]{
  def freshConstraintSystem = JustCollectCS(Seq())
}

case class JustCollectCS(cons: Seq[Constraint]) extends ConstraintSystem[JustCollectCS]{
  def state = JustCollect.state.value

  def substitution = CSubst.empty
  def isSolved = true

  def mergeSubsystem(other: JustCollectCS): JustCollectCS =
    Util.timed(state -> Statistics.mergeSolutionTime) {
      val mCons = cons ++ other.cons
      JustCollectCS(mCons)
    }

  def addNewConstraint(c: Constraint) = {
    state += Statistics.constraintCount -> 1
    Util.timed(state -> Statistics.constraintSolveTime) {
      JustCollectCS(cons :+ c)
    }
  }

  def addNewConstraints(cs: Iterable[Constraint]) = {
    state += Statistics.constraintCount -> cs.size
    Util.timed(state -> Statistics.constraintSolveTime) {
      JustCollectCS(cons ++ cs)
    }
  }

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = it

  def propagate = this

  def tryFinalize = this
}
