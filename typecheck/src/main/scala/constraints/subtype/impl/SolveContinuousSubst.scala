package constraints.subtype.impl

import constraints.subtype
import constraints.subtype.{ConstraintSystem, Type, UVar, Constraint}
import constraints.subtype.Type.Companion._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  def freshConstraintSystem = new SolveContinuousSubstCS(Map(), defaultBounds, Seq())
  def solved(s: TSubst) = new SolveContinuousSubstCS(s, defaultBounds, Seq())
  def notyet(c: Constraint) = freshConstraintSystem addNewConstraint (c)
  def never(c: Constraint) = new SolveContinuousSubstCS(Map(), defaultBounds, Seq(c))
  def system(substitution: TSubst, bounds: Map[Symbol, (LBound, UBound)], never: Seq[Constraint]) = new SolveContinuousSubstCS(substitution, bounds, never)
}

case class SolveContinuousSubstCS(substitution: TSubst, bounds: Map[Symbol, (LBound, UBound)], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  implicit val csf = SolveContinuousSubst
  import csf.state
  import csf.gen

  def notyet = {
    var cons = Seq[Constraint]()
    for ((x, (l, u)) <- bounds) {
      val join = subtype.Join(UVar(x), l.nonground ++ l.ground.toSet)
      val meet = subtype.Meet(UVar(x), u.nonground ++ u.ground.toSet)
      cons = cons :+ join :+ meet
    }
    cons
  }

  def mergeSubsystem(that: SolveContinuousSubstCS) = {
    val msubst = substitution ++ that.substitution
    var mbounds = bounds
    var mnever = never ++ that.never

    for((tv, (l1, u1)) <- that.bounds) {
      val (l2, u2) = mbounds(tv)
      val (newL, errorl) = l2 merge l1
      val (newU, erroru) = u2 merge u1
      if(errorl.nonEmpty)
        mnever = mnever :+ subtype.Join(UVar(tv), errorl)
      if(erroru.nonEmpty)
        mnever = mnever :+ subtype.Meet(UVar(tv), erroru)
      val merged = (newL, newU)
      mbounds = mbounds + (tv -> merged)
    }

    SolveContinuousSubstCS(msubst, mbounds, mnever)
  }

  def mergeApply(that: SolveContinuousSubstCS) = {
    var current = SolveContinuousSubstCS(substitution ++ that.substitution, bounds, never ++ that.never)

    for((tv, (thatL, thatU)) <- that.bounds) {
      if (bounds.isDefinedAt(tv)) {
        for (t <- thatL.nonground ++ thatL.ground.toSet)
          current = current.addLowerBound(tv, t)
        for (t <- thatU.nonground ++ thatU.ground.toSet)
          current = current.addUpperBound(tv, t)
      }
      else
        current = SolveContinuousSubstCS(current.substitution, current.bounds + (tv -> (thatL, thatU)), current.never)
    }

    current
  }

  def addNewConstraint(c: Constraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed {
      (this mergeApply c.solve(this)).trySolve
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state.value.stats.constraintCount += cons.size
    val (res, time) = Util.timed {
      cons.foldLeft(this)((cs, c) => (cs mergeApply c.solve(cs))).trySolve
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def tryFinalize = {
    val (res, time) = Util.timed {
      //set upper bounds of negative vars to Top if still undetermined and solve
      val finalbounds = bounds.map {
        case (tv, (lower, upper)) if gen.isNegative(tv) && !upper.isGround =>
          val (newUpper, _) = upper.add(subtype.Top)
          (tv, (lower, newUpper))
        case x => x
      }

      SolveContinuousSubstCS(substitution, finalbounds, never).saturateSolution
    }
    state.value.stats.finalizeTime += time
    res
  }


  private def substitutedBounds(s: TSubst) = {
    var newnever = Seq[Constraint]()
    val newbounds: Map[Symbol, (LBound,UBound)] = for ((tv, (lb, ub)) <- bounds) yield {
      val (newLb, errorl) = lb.subst(s)
      val (newUb, erroru) = ub.subst(s)
      if(errorl.nonEmpty)
        newnever  = newnever  :+ subtype.Join(UVar(tv).subst(s), errorl)
      if(erroru.nonEmpty)
        newnever  = newnever  :+ subtype.Meet(UVar(tv).subst(s), erroru)
      (tv -> (newLb, newUb))
    }
    (newbounds, newnever)
  }

  private def withSubstitutedBounds = {
    val (newbounds, newnever) = substitutedBounds(substitution)
    SolveContinuousSubstCS(substitution, newbounds, never ++ newnever)
  }

  def trySolve = saturateSolution

  private def saturateSolution = {
    var current = this.withSubstitutedBounds
    var sol = solveOnce
    while (sol.nonEmpty) {
      val subst = substitution ++ sol
      val (newbounds, newnever) = current.substitutedBounds(sol)

      var temp = csf.freshConstraintSystem

      for ((tv, (lb, ub)) <- newbounds) {
        val t = subst.getOrElse(tv, UVar(tv))
        for(tpe <- lb.ground.toSet ++ lb.nonground)
          temp = temp mergeApply tpe.subtype(t, subst)
        for(tpe <- ub.ground.toSet ++ ub.nonground)
          temp = temp mergeApply t.subtype(tpe, subst)
      }

      current = SolveContinuousSubstCS(subst, temp.bounds, current.never ++ newnever ++ temp.never)
      sol = current.solveOnce
    }
    current
  }

  private def solveOnce: TSubst = {
    var sol: TSubst = Map()
    for ((tv, (lower, upper)) <- bounds) {
      if (gen.isBipolar(tv)) {
        if (lower.isGround && upper.isGround)
          sol += tv -> lower.ground.get
      }
      else if (gen.isProperPositive(tv)) {
        if (lower.isGround)
          sol += tv -> lower.ground.get
      }
      else if (gen.isProperNegative(tv)) {
        if(upper.isGround)
          sol += tv -> upper.ground.get
      }
    }
    sol
  }

  def addLowerBound(v: Symbol, t: Type) = {
    val (lower, upper) = bounds(v)
    val (newLower, error) = lower.add(t)
    val changed = if (newLower.isGround) newLower.ground.get else t

    val newnever =
      if (error.isEmpty)
        never
      else
        never :+ subtype.Join(UVar(v), error)
    val newbounds = bounds + (v -> (newLower, upper))
    val cs = SolveContinuousSubstCS(substitution, newbounds, newnever)

    cs mergeSubsystem (subtype.Meet(changed, upper.nonground ++ upper.ground.toSet).solve(cs))
  }

  def addUpperBound(v: Symbol, t: Type) = {
    val (lower, upper) = bounds(v)
    val (newUpper, error) = upper.add(t)
    val changed = if (newUpper.isGround) newUpper.ground.get else t

    val newnever =
      if (error.isEmpty)
        never
      else
        never :+ subtype.Meet(UVar(v), error)
    val newbounds = bounds + (v -> (lower, newUpper))
    val cs = SolveContinuousSubstCS(substitution, newbounds, newnever)

    cs mergeSubsystem (subtype.Join(changed, lower.nonground ++ lower.ground.toSet).solve(cs))
  }


  def applyPartialSolution(t: Type) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U]](it: C, f: U=>Type)(implicit bf: CanBuildFrom[Iterable[U], (U, Type), C])
  = it.map(u => (u, f(u).subst(substitution)))

  def propagate = SolveContinuousSubstCS(Map(), bounds, never)
}