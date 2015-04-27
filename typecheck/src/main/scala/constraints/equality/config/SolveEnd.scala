package constraints.equality.config

import constraints.equality.Type.Companion.TSubst
import constraints.equality.{Type, EqConstraint, ConstraintSystem, ConstraintSystemFactory}
import incremental.Util

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] {
  def freshConstraintSystem = SolveEndCSRep(Seq())
  def solved(s: TSubst[SolveEndCS]) = throw new UnsupportedOperationException(s"SolveEnd cannot handle substitution $s")
  def notyet(c: EqConstraint[SolveEndCS]) = SolveEndCSRep(Seq(c))
  def never(c: EqConstraint[SolveEndCS]) = throw new UnsupportedOperationException(s"SolveEnd cannot handle unsolvable constraint $c")
}

trait SolveEndCS extends ConstraintSystem[SolveEndCS]

case class SolveEndCSRep(notyet: Seq[EqConstraint[SolveEndCS]]) extends ConstraintSystem[SolveEndCS] with SolveEndCS {
  lazy val csFactory = SolveEnd
  import csFactory.state

  def mergeSubsystem(other: SolveEndCS): SolveEndCS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      SolveEndCSRep(mnotyet)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: EqConstraint[SolveEndCS]) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(SolveEndCSRep(notyet :+ c))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cs: Iterable[EqConstraint[SolveEndCS]]) = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed(SolveEndCSRep(notyet ++ cs))
    state.value.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type[SolveEndCS]) = t

  def propagate = this

  override def tryFinalize = SolveContinuouslyCSRep(Map(), notyet, Seq()).tryFinalize
//  private def trySolve(finalize: Boolean) = {
//    var rest = notyet
//    var newSolution = substitution
//    var newNotyet = Seq[EqConstraint[SolveEndCS]]()
//    var newNever = never
//    while (!rest.isEmpty) {
//      val next = rest.head
//      rest = rest.tail
//      val wasNotyet = newNotyet ++ rest
//      val current = SolveEndCS(newSolution, wasNotyet, newNever)
//      val sol = if (finalize) next.finalize(current) else next.solve(current)
//
//      newSolution = newSolution.mapValues(_.subst(sol.substitution)) ++ sol.substitution
//      newNever = newNever ++ sol.never
//      newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
//    }
//    SolveEndCS(newSolution, newNotyet, newNever)
//  }
}