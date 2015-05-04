package constraints.normequality.impl

import constraints.CVar
import constraints.normequality.Type.Companion.TSubst
import constraints.normequality._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuously extends ConstraintSystemFactory[SolveContinuouslyCS] {
  val freshConstraintSystem = SolveContinuouslyCS(Map(), Seq(), Seq())
}

case class SolveContinuouslyCS(substitution: TSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuouslyCS] {
  def state = SolveContinuously.state.value

  def solved(s: TSubst) = {
    var current = SolveContinuouslyCS(substitution mapValues (_.subst(s)), notyet, never)
    for ((x, t2) <- s) {
      current.substitution.get(x) match {
        case None => current = SolveContinuouslyCS(current.substitution + (x -> t2.subst(current.substitution)), current.notyet, current.never)
        case Some(t1) => current = t1.unify(t2, current)
      }
    }
    current
  }

  def notyet(c: Constraint) = SolveContinuouslyCS(substitution, notyet :+ c, never)
  def never(c: Constraint) = SolveContinuouslyCS(substitution, notyet, never :+ c)
  def without(xs: Set[CVar]) = SolveContinuouslyCS(substitution -- xs, notyet, never)

  def mergeSubsystem(other: SolveContinuouslyCS): SolveContinuouslyCS = {
    val (res, time) = Util.timed {
      val msubstitution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuouslyCS(msubstitution, mnotyet, mnever)
    }
    state.stats.mergeSolutionTime += time
    res
  }

  def addNewConstraint(c: Constraint) = {
    state.stats.constraintCount += 1
    val (res, time) = Util.timed(c.solve(this))
    state.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state.stats.constraintCount += cons.size
    val (res, time) = Util.timed {
      cons.foldLeft(this)((cs, c) => c.solve(cs))
    }
    state.stats.constraintSolveTime += time
    res
  }

  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]]
    (it: C, f: U=>Type)
    (implicit bf: CanBuildFrom[Iterable[U], (U, Type), C]): C
  = it

  def propagate = this

  override def tryFinalize = {
    val (res, time) = Util.timed (trySolve(true))
    state.stats.finalizeTime += time
    res
  }

  private def trySolve(finalize: Boolean): SolveContinuouslyCS = {
    var current = this
    var stepsWithoutChange = 0
    while (!current.notyet.isEmpty) {
      val next = current.notyet.head
      val rest = current.notyet.tail
      current = SolveContinuouslyCS(current.substitution, rest, current.never)
      current =
        if (finalize)
          next.finalize(current)
        else
          next.solve(current)

      if (current.notyet.size == rest.size + 1) {
        stepsWithoutChange += 1
        if (stepsWithoutChange > rest.size + 1)
          return current
      }
      else
        stepsWithoutChange = 0
    }
    current
  }
}