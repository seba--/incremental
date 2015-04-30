package constraints.equality.impl

import constraints.equality.Type.Companion.TSubst
import constraints.equality._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  val freshConstraintSystem = SolveContinuousSubstCS(Map(), Seq(), Seq())
  def solved(s: TSubst) = SolveContinuousSubstCS(s, Seq(), Seq())
  def notyet(c: Constraint) = SolveContinuousSubstCS(Map(), Seq(c), Seq())
  def never(c: Constraint) = SolveContinuousSubstCS(Map(), Seq(), Seq(c))
}

case class SolveContinuousSubstCS(substitution: TSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstCS] {
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

  def addNewConstraint(c: Constraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeApply c.solve(this, SolveContinuousSubst))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[Constraint]): SolveContinuousSubstCS = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(this)((sol, c) => sol mergeApply c.solve(sol, SolveContinuousSubst))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U]]
    (it: C, f: U=>Type)
    (implicit bf: CanBuildFrom[Iterable[U], (U, Type), C]): C
  = it.map(u => (u, f(u).subst(substitution)))


  def propagate = SolveContinuousSubstCS(Map(), notyet.map(_.subst(substitution)), never.map(_.subst(substitution)))

  override def tryFinalize =
    SolveContinuously.state.withValue(state.value) {
      SolveContinuouslyCS(substitution, notyet, never).tryFinalize
    }
}