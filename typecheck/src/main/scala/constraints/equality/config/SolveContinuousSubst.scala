package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, EqConstraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  val freshConstraintSystem = SolveContinuousSubstCS(Map(), Seq(), Seq())
  def solved(s: TSubst) = SolveContinuousSubstCS(s, Seq(), Seq())
  def notyet(c: EqConstraint) = SolveContinuousSubstCS(Map(), Seq(c), Seq())
  def never(c: EqConstraint) = SolveContinuousSubstCS(Map(), Seq(), Seq(c))
}

case class SolveContinuousSubstCS(substitution: TSubst, notyet: Seq[EqConstraint], never: Seq[EqConstraint]) extends ConstraintSystem[SolveContinuousSubstCS] {
  import SolveContinuousSubst.state

  def mergeSubsystem(other: SolveContinuousSubstCS): SolveContinuousSubstCS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuousSubstCS(Map(), mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def mergeApply(other: SolveContinuousSubstCS): SolveContinuousSubstCS = {
    val (res, time) = Util.timed {
      var msolution = substitution mapValues (_.subst(other.substitution))
      val mnotyet = notyet ++ other.notyet
      var mnever = never ++ other.never

      for ((x, t2) <- other.substitution) {
        msolution.get(x) match {
          case None => msolution += x -> t2.subst(msolution)
          case Some(t1) =>
            val usol = t1.unify(t2, msolution)(SolveContinuousSubst)
            msolution = msolution.mapValues(_.subst(usol.substitution)) ++ usol.substitution
            mnever = mnever ++ usol.never
        }
      }

      SolveContinuousSubstCS(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: EqConstraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeApply c.solve(this, SolveContinuousSubst))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[EqConstraint]): SolveContinuousSubstCS = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(this)((sol, c) => sol mergeApply c.solve(sol, SolveContinuousSubst))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type) = t.subst(substitution)

  def propagate = SolveContinuousSubstCS(Map(), notyet, never)

  override def tryFinalize = SolveContinuouslyCS(substitution, notyet, never).tryFinalize
}