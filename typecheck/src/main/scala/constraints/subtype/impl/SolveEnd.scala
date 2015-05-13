package constraints.subtype.impl

import constraints.StatKeys._
import constraints.{StatKeys$, CVar, subtype}
import constraints.subtype._
import constraints.subtype.CSubst.CSubst
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCS(Seq())
  def solved(s: CSubst) = throw new UnsupportedOperationException(s"SolveAllEnd cannot handle substitution $s")
  def notyet(c: Constraint) = SolveEndCS(Seq(c))
  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveAllEnd cannot handle unsolvable constraint $c")
}

case class SolveEndCS(notyet: Seq[Constraint]) extends ConstraintSystem[SolveEndCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  def state = SolveEnd.state
  def stats = SolveEnd.state.stats

  def substitution = Map()
  def never = Seq[Constraint]()

  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveEnd cannot handle unsolvable constraint $c")

  def mergeSubsystem(that: SolveEndCS) =
    stats(MergeSolution) {
      val mnotyet = notyet ++ that.notyet
      SolveEndCS(mnotyet)
    }


  override def addNewConstraint(c: Constraint) = {
    stats.addToConstraintCount(1)
    stats(SolveConstraint) {
      SolveEndCS(notyet :+ c)
    }
  }

  override def addNewConstraints(cons: Iterable[Constraint]) = {
    stats.addToConstraintCount(cons.size)
    stats(SolveConstraint) {
      SolveEndCS(notyet ++ cons)
    }
  }

  def tryFinalize = {
    SolveContinuously.state = state
    (SolveContinuously.freshConstraintSystem addNewConstraints notyet).tryFinalize
  }

  def addLowerBound(v: CVar[Type], t: Type) = SolveEndCS(notyet :+ subtype.Subtype(t, UVar(v)))

  def addUpperBound(v: CVar[Type], t: Type) = SolveEndCS(notyet :+ subtype.Subtype(UVar(v), t))

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = it

  def propagate = this
}