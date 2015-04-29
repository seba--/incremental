package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, Constraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCS(Seq())
  def solved(s: TSubst) = throw new UnsupportedOperationException(s"SolveEnd cannot handle substitution $s")
  def notyet(c: Constraint) = SolveEndCS(Seq(c))
  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveEnd cannot handle unsolvable constraint $c")
}

case class SolveEndCS(notyet: Seq[Constraint]) extends ConstraintSystem[SolveEndCS] {
  import SolveEnd.state

  def substitution = Map()
  def never = Seq()

  def mergeSubsystem(other: SolveEndCS): SolveEndCS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      SolveEndCS(mnotyet)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: Constraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(SolveEndCS(notyet :+ c))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[Constraint]) = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed(SolveEndCS(notyet ++ cs))
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]]
    (it: C, f: U=>Type)
    (implicit bf: CanBuildFrom[Iterable[U], (U, Type), C]): C
  = it


  def propagate = this

  override def tryFinalize =
    SolveContinuously.state.withValue(state.value) {
      SolveContinuouslyCS(Map(), notyet, Seq()).tryFinalize
    }
}