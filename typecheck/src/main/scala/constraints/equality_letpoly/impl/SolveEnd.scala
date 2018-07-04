package constraints.equality_letpoly.impl

import constraints.{Statistics, CVar}
import constraints.equality_letpoly.CSubst.CSubst
import constraints.equality_letpoly._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCS(Seq())
}

case class SolveEndCS(notyet: Seq[Constraint]) extends ConstraintSystem[SolveEndCS] {
  def state = SolveEnd.state.value

  def substitution = CSubst.empty
  def never = Seq()

  def solved(s: CSubst) = throw new UnsupportedOperationException(s"SolveEnd cannot handle substitution $s")
  def notyet(c: Constraint) = SolveEndCS(notyet :+ c)
  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveEnd cannot handle unsolvable constraint $c")
  def without(xs: Set[CVar[_]]) = this


  def mergeSubsystem(other: SolveEndCS): SolveEndCS =
    Util.timed(state -> Statistics.mergeSolutionTime) {
      val mnotyet = notyet ++ other.notyet
      SolveEndCS(mnotyet)
    }

  def addcompatibleCons(t1 : Type, t2 : Type) = this


  def addNewConstraint(c: Constraint) = {
    state += Statistics.constraintCount -> 1
    Util.timed(state -> Statistics.constraintSolveTime) {
      SolveEndCS(notyet :+ c)
    }
  }

  def addNewConstraints(cs: Iterable[Constraint]) = {
    state += Statistics.constraintCount -> cs.size
    Util.timed(state -> Statistics.constraintSolveTime) {
      SolveEndCS(notyet ++ cs)
    }
  }

  def shouldApplySubst: Boolean = false

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = it


  def propagate = this

  override def tryFinalize =
    SolveContinuously.state.withValue(state) {
      SolveContinuouslyCS(CSubst.empty, notyet, Seq(), Map()).tryFinalize
    }
}