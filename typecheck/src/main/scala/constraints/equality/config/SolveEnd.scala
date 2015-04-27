package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, EqConstraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {

}

abstract class SolveEndCS extends ConstraintSystem[SolveEndCS] {
  val csFactory = SolveEnd
}

case class SolveEndCSIntermediate(notyet: Seq[EqConstraint[SolveEndCS]]) extends SolveEndCS {
  import csFactory.state
  val substitution = Map()
  val never = Seq()

  def mergeSubsystem(other: SolveEndCS): SolveEndCS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      SolveEndCSIntermediate(mnotyet)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }


  def addNewConstraint(c: EqConstraint[SolveEndCS]) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(SolveEndCSIntermediate(notyet :+ c))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[EqConstraint[SolveEndCS]]) = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed(SolveEndCSIntermediate(notyet ++ cs))
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type[SolveEndCS]) = t

  def propagate = this

  def tryFinalize = SolveEndCSFinal(Map(), notyet, never).tryFinalize
}

case class SolveEndCSFinal(substitution: TSubst[SolveEndCS], notyet: Seq[EqConstraint[SolveEndCS]], never: Seq[EqConstraint[SolveEndCS]]) extends SolveEndCS {
  import csFactory.state

  def tryFinalize = trySolve(true)
  private def trySolve(finalize: Boolean) = {
    var rest = notyet
    var newSolution = substitution
    var newNotyet = Seq[EqConstraint[SolveEndCS]]()
    var newNever = never
    while (!rest.isEmpty) {
      val next = rest.head
      rest = rest.tail
      val wasNotyet = newNotyet ++ rest
      val current = SolveEndCSFinal(newSolution, wasNotyet, newNever)
      val sol = if (finalize) next.finalize(current) else next.solve(current)

      newSolution = newSolution.mapValues(_.subst(sol.substitution)) ++ sol.substitution
      newNever = newNever ++ sol.never
      newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
    }
    SolveEndCSFinal(newSolution, newNotyet, newNever)
  }
}