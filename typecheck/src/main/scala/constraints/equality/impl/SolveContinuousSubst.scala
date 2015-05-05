package constraints.equality.impl

import constraints.{CTermBase, CSubst, CVar}
import constraints.equality._
import constraints.equality.CSubst.CSubst
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  val freshConstraintSystem = SolveContinuousSubstCS(CSubst.empty, Seq(), Seq())
}

case class SolveContinuousSubstCS(substitution: CSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstCS] {
  def state = SolveContinuousSubst.state.value

  def solved(s: CSubst) = {
    var current = SolveContinuousSubstCS(substitution mapValues (x => x.subst(s)), notyet, never)
    for ((x, t2) <- s) {
      current.substitution.get(x) match {
        case None => current = SolveContinuousSubstCS(current.substitution + (x -> t2.subst(current.substitution)), current.notyet, current.never)
        case Some(t1) => current = t1.compatibleWith(t2).solve(current)
      }
    }
    current
  }


  def notyet(c: Constraint) = SolveContinuousSubstCS(substitution, notyet :+ c, never)
  def never(c: Constraint) = SolveContinuousSubstCS(substitution, notyet, never :+ c)
  def without(xs: Set[CVar[_]]) = SolveContinuousSubstCS(substitution -- xs, notyet, never)

  def mergeSubsystem(other: SolveContinuousSubstCS): SolveContinuousSubstCS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuousSubstCS(CSubst.empty, mnotyet, mnever)
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

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = it.map(u => (u, f(u).subst(substitution)))


  def propagate = SolveContinuousSubstCS(CSubst.empty, notyet.map(_.subst(substitution)), never.map(_.subst(substitution)))

  override def tryFinalize =
    SolveContinuously.state.withValue(state) {
      SolveContinuouslyCS(substitution, notyet, never).tryFinalize
    }
}