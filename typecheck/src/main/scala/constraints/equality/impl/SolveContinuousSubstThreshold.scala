package constraints.equality.impl

import constraints.{Statistics, CVar}
import constraints.equality._
import constraints.equality.CSubst.CSubst
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubstThreshold extends ConstraintSystemFactory[SolveContinuousSubstThresholdCS] {
  var threshold = 10

  val freshConstraintSystem = SolveContinuousSubstThresholdCS(CSubst.empty, Seq(), Seq())
}

case class SolveContinuousSubstThresholdCS(substitution: CSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstThresholdCS] {
  import SolveContinuousSubstThreshold.threshold

  def state = SolveContinuousSubstThreshold.state.value

  def solved(s: CSubst) = {
    var current = SolveContinuousSubstThresholdCS(substitution mapValues (_.subst(s)), notyet, never)
    for ((x, t2) <- s) {
      current.substitution.get(x) match {
        case None => current = SolveContinuousSubstThresholdCS(current.substitution + (x -> t2.subst(current.substitution)), current.notyet, current.never)
        case Some(t1) => current = t1.compatibleWith(t2).solve(current)
      }
    }
    current
  }

  def notyet(c: Constraint) = SolveContinuousSubstThresholdCS(substitution, notyet :+ c, never)
  def never(c: Constraint) = SolveContinuousSubstThresholdCS(substitution, notyet, never :+ c)
  def without(xs: Set[CVar[_]]) = SolveContinuousSubstThresholdCS(substitution -- xs, notyet, never)

  lazy val trigger = substitution.size >= threshold

  def mergeSubsystem(other: SolveContinuousSubstThresholdCS): SolveContinuousSubstThresholdCS =
    Util.timed(state -> Statistics.mergeSolutionTime) {
      val msubstitution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuousSubstThresholdCS(msubstitution, mnotyet, mnever)
    }

  def addNewConstraint(c: Constraint) = {
    state += Statistics.constraintCount -> 1
    Util.timed(state -> Statistics.constraintSolveTime){
      c.solve(this)
    }
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state += Statistics.constraintCount -> cons.size
    Util.timed(state -> Statistics.constraintSolveTime) {
      cons.foldLeft(this)((cs, c) => c.solve(cs))
    }
  }

  def shouldApplySubst: Boolean = trigger
  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) =
    if (trigger)
      t.subst(substitution)
    else
      t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = if (trigger)
      it.map(u => (u, f(u).subst(substitution)))
    else
      it


  def propagate =
    if (trigger)
      SolveContinuousSubstThresholdCS(CSubst.empty, notyet.map(_.subst(substitution)), never.map(_.subst(substitution)))
    else
      this

  override def tryFinalize =
    SolveContinuously.state.withValue(state) {
      SolveContinuouslyCS(substitution, notyet, never).tryFinalize
    }
}