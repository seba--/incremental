package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, EqConstraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

object SolveContinuousSubstThreshold extends ConstraintSystemFactory[SolveContinuousSubstThresholdCS] {
  var threshold = 2

  val freshConstraintSystem = SolveContinuousSubstThresholdCS(Map(), Seq(), Seq())
  def solved(s: TSubst) = SolveContinuousSubstThresholdCS(s, Seq(), Seq())
  def notyet(c: EqConstraint) = SolveContinuousSubstThresholdCS(Map(), Seq(c), Seq())
  def never(c: EqConstraint) = SolveContinuousSubstThresholdCS(Map(), Seq(), Seq(c))
}

case class SolveContinuousSubstThresholdCS(substitution: TSubst, notyet: Seq[EqConstraint], never: Seq[EqConstraint]) extends ConstraintSystem[SolveContinuousSubstThresholdCS] {
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

  def addNewConstraint(c: EqConstraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeApply c.solve(this, SolveContinuousSubstThreshold))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[EqConstraint]): SolveContinuousSubstThresholdCS = {
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

  def propagate =
    if (trigger)
      SolveContinuousSubstThresholdCS(Map(), notyet, never)
    else
      this

  override def tryFinalize = SolveContinuouslyCS(substitution, notyet, never).tryFinalize
}