package constraints.normequality.impl

import constraints.CVar
import constraints.normequality._
import constraints.normequality.CSubst.CSubst
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubstThreshold extends ConstraintSystemFactory[SolveContinuousSubstThresholdCS] {
  var threshold = 10

  val freshConstraintSystem = SolveContinuousSubstThresholdCS(CSubst.empty, Seq(), Seq())
}

case class SolveContinuousSubstThresholdCS(substitution: CSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstThresholdCS] {
  import SolveContinuousSubstThreshold.threshold

  def state = SolveContinuousSubstThreshold.state.value

  def solved(s: CSubst) = {
    var current = SolveContinuousSubstThresholdCS(substitution mapValues (_.subst(s)), notyet, never)
    for ((x, t2) <- s) {
      current.substitution.get(x) match {
        case None => current = SolveContinuousSubstThresholdCS(current.substitution + (x -> t2.subst(current.substitution)), current.notyet, current.never)
        case Some(t1) => current = t1.compatibleWith(t2).solve(current)
      }
    }
    current
  }

  def notyet(c: Constraint) = SolveContinuousSubstThresholdCS(substitution, notyet :+ c, never)
  def never(c: Constraint) = SolveContinuousSubstThresholdCS(substitution, notyet, never :+ c)
  def without(xs: Set[CVar[_]]) = SolveContinuousSubstThresholdCS(substitution -- xs, notyet, never)

  lazy val trigger = substitution.size >= threshold

  def mergeSubsystem(other: SolveContinuousSubstThresholdCS): SolveContinuousSubstThresholdCS = {
    val (res, time) = Util.timed {
      val msubstitution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuousSubstThresholdCS(msubstitution, mnotyet, mnever)
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

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) =
    if (trigger)
      t.subst(substitution)
    else
      t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
  (it: C, f: U=>CT)
  (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = if (trigger)
    it.map(u => (u, f(u).subst(substitution)))
  else
    it


  def propagate =
    if (trigger)
      SolveContinuousSubstThresholdCS(CSubst.empty, notyet.map(_.subst(substitution)), never.map(_.subst(substitution)))
    else
      this

  override def tryFinalize =
    SolveContinuously.state.withValue(state) {
      SolveContinuouslyCS(substitution, notyet, never).tryFinalize
    }
}