package constraints.equality.impl

import constraints.{Statistics, CVar}
import constraints.equality.CSubst.CSubst
import constraints.equality._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCS(Seq())
}

case class SolveEndCS(notyet: Seq[Constraint]) extends ConstraintSystem[SolveEndCS] {
  def state = SolveEnd.state
  def stats = SolveEnd.state.stats

  def substitution = CSubst.empty
  def never = Seq()

  def solved(s: CSubst) = throw new UnsupportedOperationException(s"SolveEnd cannot handle substitution $s")
  def notyet(c: Constraint) = SolveEndCS(notyet :+ c)
  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveEnd cannot handle unsolvable constraint $c")
  def without(xs: Set[CVar[_]]) = this


  def mergeSubsystem(other: SolveEndCS): SolveEndCS =
    stats.mergeSolutionTimed {
      val mnotyet = notyet ++ other.notyet
      SolveEndCS(mnotyet)
    }

  def addNewConstraint(c: Constraint) = {
    stats.addToConstraintCount(1)
    stats.constraintSolveTimed {
      SolveEndCS(notyet :+ c)
    }
  }

  def addNewConstraints(cs: Iterable[Constraint]) = {
    stats.addToConstraintCount(cs.size)
    stats.constraintSolveTimed {
      SolveEndCS(notyet ++ cs)
    }
  }

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = it


  def propagate = this

  override def tryFinalize = {
    SolveContinuously.state = state
    SolveContinuouslyCS(CSubst.empty, notyet, Seq()).tryFinalize
  }
}