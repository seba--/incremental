package constraints.equality.impl

//import constraints._

import constraints.CVar
import constraints.equality.Constraint
import constraints.equality.ConstraintSystem
import constraints.equality.ConstraintSystemFactory
import constraints.equality._
import constraints.equality.CSubst.CSubst

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  val freshConstraintSystem = SolveContinuousSubstCS(CSubst.empty, Seq(), Seq())
}

case class SolveContinuousSubstCS(substitution: CSubst, notyet: Seq[Constraint], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstCS] {
  import constraints.StatKeys._

  def state = SolveContinuousSubst.state
  def stats = SolveContinuousSubst.state.stats

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

  def mergeSubsystem(other: SolveContinuousSubstCS): SolveContinuousSubstCS =
    stats(MergeSolution) {
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      SolveContinuousSubstCS(CSubst.empty, mnotyet, mnever)
    }

  def addNewConstraint(c: Constraint) = {
    stats.addToConstraintCount(1)
    stats(SolveConstraint) {
      c.solve(this)
    }
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    stats.addToConstraintCount(cons.size)
    stats(SolveConstraint) {
      cons.foldLeft(this)((cs, c) => c.solve(cs))
    }
  }

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = it.map(u => (u, f(u).subst(substitution)))


  def propagate = SolveContinuousSubstCS(CSubst.empty, notyet.map(_.subst(substitution)), never.map(_.subst(substitution)))

  override def tryFinalize = {
    SolveContinuously.state = state
    SolveContinuouslyCS(substitution, notyet, never).tryFinalize
  }
}