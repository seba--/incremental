package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, EqConstraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

object SolveContinuously extends ConstraintSystemFactory[SolveContinuouslyCS] {
  val freshConstraintSystem = SolveContinuouslyCSRep(Map(), Seq(), Seq())
  def solved(s: TSubst[SolveContinuouslyCS]) = SolveContinuouslyCSRep(s, Seq(), Seq())
  def notyet(c: EqConstraint[SolveContinuouslyCS]) = SolveContinuouslyCSRep(Map(), Seq(c), Seq())
  def never(c: EqConstraint[SolveContinuouslyCS]) = SolveContinuouslyCSRep(Map(), Seq(), Seq(c))
}

trait SolveContinuouslyCS extends ConstraintSystem[SolveContinuouslyCS]

case class SolveContinuouslyCSRep(substitution: TSubst[SolveContinuouslyCS], notyet: Seq[EqConstraint[SolveContinuouslyCS]], never: Seq[EqConstraint[SolveContinuouslyCS]]) extends ConstraintSystem[SolveContinuouslyCS] with SolveContinuouslyCS {
  lazy val csFactory = SolveContinuously
  import csFactory.state

  def mergeSubsystem(other: SolveContinuouslyCS): SolveContinuouslyCS = {
    val (res, time) = Util.timed {
      val msubstitution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuouslyCSRep(msubstitution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def mergeApply(other: SolveContinuouslyCS): SolveContinuouslyCSRep = {
    val (res, time) = Util.timed {
      var msolution = substitution mapValues (_.subst(other.substitution))
      val mnotyet = notyet ++ other.notyet
      var mnever = never ++ other.never

      for ((x, t2) <- other.substitution) {
        msolution.get(x) match {
          case None => msolution += x -> t2.subst(msolution)
          case Some(t1) =>
            val usol = t1.unify(t2, msolution)
            msolution = msolution.mapValues(_.subst(usol.substitution)) ++ usol.substitution
            mnever = mnever ++ usol.never
        }
      }

      SolveContinuouslyCSRep(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: EqConstraint[SolveContinuouslyCS]) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeApply c.solve(this))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[EqConstraint[SolveContinuouslyCS]]): SolveContinuouslyCS = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(this)((sol, c) => sol mergeApply c.solve(sol))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type[SolveContinuouslyCS]) = t

  def propagate = this

  override def tryFinalize = trySolve(true)
  private def trySolve(finalize: Boolean) = {
    var rest = notyet
    var newSolution = substitution
    var newNotyet = Seq[EqConstraint[SolveContinuouslyCS]]()
    var newNever = never
    while (!rest.isEmpty) {
      val next = rest.head
      rest = rest.tail
      val wasNotyet = newNotyet ++ rest
      val current = SolveContinuouslyCSRep(newSolution, wasNotyet, newNever)
      val sol = if (finalize) next.finalize(current) else next.solve(current)

      newSolution = newSolution.mapValues(_.subst(sol.substitution)) ++ sol.substitution
      newNever = newNever ++ sol.never
      newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
    }
    SolveContinuouslyCSRep(newSolution, newNotyet, newNever)
  }
}