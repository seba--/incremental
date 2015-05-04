package constraints.normequality.impl

import constraints.normequality.Type.Companion.TSubst
import constraints.normequality.{Type, Constraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCS(Seq())
}

case class SolveEndCS(notyet: Seq[Constraint]) extends ConstraintSystem[SolveEndCS] {
  def state = SolveEnd.state.value

  def substitution = Map()
  def never = Seq()

  def solved(s: TSubst) = throw new UnsupportedOperationException(s"SolveEnd cannot handle substitution $s")
  def notyet(c: Constraint) = SolveEndCS(notyet :+ c)
  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveEnd cannot handle unsolvable constraint $c")
  def without(xs: Set[Symbol]) = this


  def mergeSubsystem(other: SolveEndCS): SolveEndCS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      SolveEndCS(mnotyet)
    }
    state.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: Constraint) = {
    state.stats.constraintCount += 1
    val (res, time) = Util.timed(SolveEndCS(notyet :+ c))
    state.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[Constraint]) = {
    state.stats.constraintCount += cs.size
    val (res, time) = Util.timed(SolveEndCS(notyet ++ cs))
    state.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]]
    (it: C, f: U=>Type)
    (implicit bf: CanBuildFrom[Iterable[U], (U, Type), C]): C
  = it


  def propagate = this

  override def tryFinalize =
    SolveContinuously.state.withValue(state) {
      SolveContinuouslyCS(Map(), notyet, Seq()).tryFinalize
    }
}