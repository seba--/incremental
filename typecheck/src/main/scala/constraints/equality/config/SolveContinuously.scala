package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, EqConstraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuously extends ConstraintSystemFactory[SolveContinuouslyCS] {
  val freshConstraintSystem = SolveContinuouslyCS(Map(), Seq(), Seq())
  def solved(s: TSubst) = SolveContinuouslyCS(s, Seq(), Seq())
  def notyet(c: EqConstraint) = SolveContinuouslyCS(Map(), Seq(c), Seq())
  def never(c: EqConstraint) = SolveContinuouslyCS(Map(), Seq(), Seq(c))
}

case class SolveContinuouslyCS(substitution: TSubst, notyet: Seq[EqConstraint], never: Seq[EqConstraint]) extends ConstraintSystem[SolveContinuouslyCS] {
  import SolveContinuously.state

  def mergeSubsystem(other: SolveContinuouslyCS): SolveContinuouslyCS = {
    val (res, time) = Util.timed {
      val msubstitution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuouslyCS(msubstitution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def mergeApply(other: SolveContinuouslyCS): SolveContinuouslyCS = {
    val (res, time) = Util.timed {
      var msolution = substitution mapValues (_.subst(other.substitution))
      val mnotyet = notyet ++ other.notyet
      var mnever = never ++ other.never

      for ((x, t2) <- other.substitution) {
        msolution.get(x) match {
          case None => msolution += x -> t2.subst(msolution)
          case Some(t1) =>
            val usol = t1.unify(t2, msolution)(SolveContinuously)
            msolution = msolution.mapValues(_.subst(usol.substitution)) ++ usol.substitution
            mnever = mnever ++ usol.never
        }
      }

      SolveContinuouslyCS(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: EqConstraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeApply c.solve(this, SolveContinuously))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[EqConstraint]): SolveContinuouslyCS = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(this)((sol, c) => sol mergeApply c.solve(sol, SolveContinuously))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]]
    (it: C, f: U=>Type)
    (implicit bf: CanBuildFrom[Iterable[U], (U, Type), C]): C
  = it

  def propagate = this

  override def tryFinalize = trySolve(true)
  private def trySolve(finalize: Boolean) = {
    var rest = notyet
    var newSolution = substitution
    var newNotyet = Seq[EqConstraint]()
    var newNever = never
    while (!rest.isEmpty) {
      val next = rest.head
      rest = rest.tail
      val wasNotyet = newNotyet ++ rest
      val current = SolveContinuouslyCS(newSolution, wasNotyet, newNever)
      val sol = if (finalize) next.finalize(current, SolveContinuously) else next.solve(current, SolveContinuously)

      newSolution = newSolution.mapValues(_.subst(sol.substitution)) ++ sol.substitution
      newNever = newNever ++ sol.never
      newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
    }
    SolveContinuouslyCS(newSolution, newNotyet, newNever)
  }
}