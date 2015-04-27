package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, EqConstraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  val emptySolution = new SolveEndCS(Map(), Seq(), Seq())
  def solved(s: TSubst[SolveEndCS) = new SolveEndCS(s, Seq(), Seq())
  def notyet(c: EqConstraint[SolveEndCS]) = new SolveEndCS(Map(), Seq(c), Seq())
  def never(c: EqConstraint[SolveEndCS]) = new SolveEndCS(Map(), Seq(), Seq(c))
}

class SolveEndCS(substitution: TSubst[SolveEndCS], notyet: Seq[EqConstraint[SolveEndCS]], never: Seq[EqConstraint[SolveEndCS]]) extends ConstraintSystem[SolveEndCS](substitution, notyet, never) {
  lazy val csFactory = SolveEnd
  import csFactory.state

  def mergeSubsystem(other: SolveEndCS): SolveEndCS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      new SolveEndCS(substitution, mnotyet, never)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: EqConstraint[SolveEndCS]) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(new SolveEndCS(substitution, notyet :+ c, never))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[EqConstraint[SolveEndCS]]) = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed(new SolveEndCS(substitution, notyet ++ cs, never))
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type[SolveEndCS]) = t

  def propagate = this

  override def tryFinalize = trySolve(true)
  private def trySolve(finalize: Boolean) = {
    var rest = notyet
    var newSolution = substitution
    var newNotyet = Seq[EqConstraint[SolveEndCS]]()
    var newNever = never
    while (!rest.isEmpty) {
      val next = rest.head
      rest = rest.tail
      val wasNotyet = newNotyet ++ rest
      val current = new SolveEndCS(newSolution, wasNotyet, newNever)
      val sol = if (finalize) next.finalize(current) else next.solve(current)

      newSolution = newSolution.mapValues(_.subst(sol.substitution)) ++ sol.substitution
      newNever = newNever ++ sol.never
      newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
    }
    new SolveEndCS(newSolution, newNotyet, newNever)
  }
}