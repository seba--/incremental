package constraints.subtype.bounds

import constraints.subtype
import constraints.subtype.{ConstraintSystem, Type, UVar, Constraint}
import constraints.subtype.Type.Companion._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCS(Seq())
  def solved(s: TSubst) = throw new UnsupportedOperationException(s"SolveAllEnd cannot handle substitution $s")
  def notyet(c: Constraint) = SolveEndCS(Seq(c))
  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveAllEnd cannot handle unsolvable constraint $c")
}

case class SolveEndCS(notyet: Seq[Constraint]) extends ConstraintSystem[SolveEndCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  implicit val csf = SolveEnd
  import csf.state

  val substitution: TSubst = Map()
  val never = Seq[Constraint]()

  def mergeSubsystem(that: SolveEndCS) = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ that.notyet
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

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state.value.stats.constraintCount += cons.size
    val (res, time) = Util.timed(SolveEndCS(notyet ++ cons))
    state.value.stats.constraintSolveTime += time
    res
  }

  def tryFinalize =
    SolveContinuously.state.withValue(state.value) {
      (SolveContinuously.freshConstraintSystem addNewConstraints notyet).tryFinalize
    }

  def addLowerBound(v: Symbol, t: Type) = SolveEndCS(notyet :+ subtype.Subtype(t, UVar(v)))

  def addUpperBound(v: Symbol, t: Type) = SolveEndCS(notyet :+ subtype.Subtype(UVar(v), t))

  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]](it: C, f: U=>Type)(implicit bf: CanBuildFrom[Iterable[U], (U, Type), C])
  = it

  def propagate = this
}