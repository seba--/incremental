package constraints.subtype.impl

import constraints.{Statistics, CVar, subtype}
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

  def state = SolveEnd.state.value

  def substitution = Map()
  def never = Seq[Constraint]()

  def never(c: Constraint) = throw new UnsupportedOperationException(s"SolveEnd cannot handle unsolvable constraint $c")

  def mergeSubsystem(that: SolveEndCS) =
    Util.timed(state -> Statistics.mergeSolutionTime) {
      val mnotyet = notyet ++ that.notyet
      SolveEndCS(mnotyet)
    }


  override def addNewConstraint(c: Constraint) = {
    state += Statistics.constraintCount -> 1
    Util.timed(state -> Statistics.constraintSolveTime) {
      SolveEndCS(notyet :+ c)
    }
  }

  override def addNewConstraints(cons: Iterable[Constraint]) = {
    state += Statistics.constraintCount -> cons.size
    Util.timed(state -> Statistics.constraintSolveTime) {
      SolveEndCS(notyet ++ cons)
    }
  }

  def tryFinalize =
    SolveContinuously.state.withValue(state) {
      (SolveContinuously.freshConstraintSystem addNewConstraints notyet).tryFinalize
    }

  def addLowerBound(v: CVar[Type], t: Type) = SolveEndCS(notyet :+ subtype.Subtype(t, UCName(v)))

  def addUpperBound(v: CVar[Type], t: Type) = SolveEndCS(notyet :+ subtype.Subtype(UCName(v), t))

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = it

  def propagate = this
}