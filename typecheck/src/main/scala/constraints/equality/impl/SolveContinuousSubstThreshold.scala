package constraints.equality.impl

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, Constraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubstThreshold extends ConstraintSystemFactory[SolveContinuousSubstThresholdCS] {
  var threshold = 2

  val freshConstraintSystem = SolveContinuousSubstThresholdCS(Map(), Seq(), Seq())
  def solved(s: TSubst) = SolveContinuousSubstThresholdCS(s, Seq(), Seq())
  def notyet(c: Constraint) = SolveContinuousSubstThresholdCS(Map(), Seq(c), Seq())
  def never(c: Constraint) = SolveContinuousSubstThresholdCS(Map(), Seq(), Seq(c))
}

case class SolveContinuousSubstThresholdCS(substitution: TSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstThresholdCS] {
  import SolveContinuousSubstThreshold.state
  import SolveContinuousSubstThreshold.threshold

  lazy val trigger = substitution.size >= threshold

  def mergeSubsystem(other: SolveContinuousSubstThresholdCS): SolveContinuousSubstThresholdCS = {
    val (res, time) = Util.timed {
      val msubstitution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuousSubstThresholdCS(msubstitution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def mergeApply(other: SolveContinuousSubstThresholdCS): SolveContinuousSubstThresholdCS = {
    val (res, time) = Util.timed {
      var msolution = substitution mapValues (_.subst(other.substitution))
      val mnotyet = notyet ++ other.notyet
      var mnever = never ++ other.never

      for ((x, t2) <- other.substitution) {
        msolution.get(x) match {
          case None => msolution += x -> t2.subst(msolution)
          case Some(t1) =>
            val usol = t1.unify(t2, msolution)(SolveContinuousSubstThreshold)
            msolution = msolution.mapValues(_.subst(usol.substitution)) ++ usol.substitution
            mnever = mnever ++ usol.never
        }
      }

      SolveContinuousSubstThresholdCS(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: Constraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeApply c.solve(this, SolveContinuousSubstThreshold))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[Constraint]): SolveContinuousSubstThresholdCS = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(this)((sol, c) => sol mergeApply c.solve(sol, SolveContinuousSubstThreshold))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type) =
    if (trigger)
      t.subst(substitution)
    else
      t

  def applyPartialSolutionIt[U, C <: Iterable[U]]
    (it: C, f: U=>Type)
    (implicit bf: CanBuildFrom[Iterable[U], (U, Type), C]): C
  = if (trigger)
      it.map(u => (u, f(u).subst(substitution)))
    else
      it


  def propagate =
    if (trigger)
      SolveContinuousSubstThresholdCS(Map(), notyet.map(_.subst(substitution)), never.map(_.subst(substitution)))
    else
      this

  override def tryFinalize =
    SolveContinuously.state.withValue(state.value) {
      SolveContinuouslyCS(substitution, notyet, never).tryFinalize
    }
}