package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuously extends ConstraintSystemFactory[SolveContinuouslyCS] {
  val freshConstraintSystem = SolveContinuouslyCS(Map(), Seq(), Seq())
  def solved(s: TSubst) = SolveContinuouslyCS(s, Seq(), Seq())
  def notyet(c: Constraint) = SolveContinuouslyCS(Map(), Seq(c), Seq())
  def never(c: Constraint) = SolveContinuouslyCS(Map(), Seq(), Seq(c))
}

case class SolveContinuouslyCS(substitution: TSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuouslyCS] {
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
      var mnotyet = notyet ++ other.notyet
      var mnever = never ++ other.never

      for ((x, t2) <- other.substitution) {
        msolution.get(x) match {
          case None => msolution += x -> t2.subst(msolution)
          case Some(t1) =>
            val usol = t1.unify(t2, msolution)(SolveContinuously)
            msolution = msolution.mapValues(_.subst(usol.substitution)) ++ usol.substitution
            mnotyet = mnotyet ++ usol.notyet
            mnever = mnever ++ usol.never
        }
      }

      SolveContinuouslyCS(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: Constraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeApply c.solve(this, SolveContinuously))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[Constraint]): SolveContinuouslyCS = {
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

  private def trySolve(finalize: Boolean): SolveContinuouslyCS = {
    var current = this
//    var rest = notyet
//    var newSolution = substitution
//    var newNotyet = Seq[Constraint]()
//    var newNever = never
    while (!current.notyet.isEmpty) {
      val next = current.notyet.head
      val rest = current.notyet.tail

      current = SolveContinuouslyCS(current.substitution, rest, current.never)

      val sol = if (finalize) next.finalize(current, SolveContinuously) else next.solve(current, SolveContinuously)

      if (sol.notyet.exists(c => rest.contains(c)))
        return SolveContinuouslyCS(current.substitution, next +: rest, current.never)

      current = current mergeApply sol
//      newSolution = mergedSol.substitution
//      newNever = newNever ++ sol.never
//      newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
    }
    current
  }
}