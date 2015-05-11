package constraints.subtype.impl

import constraints.{Statistics, CVar, subtype}
import constraints.subtype._
import constraints.subtype.CSubst.CSubst
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubstThreshold extends ConstraintSystemFactory[SolveContinuousSubstThresholdCS] {
  var threshold = 10

  def freshConstraintSystem = new SolveContinuousSubstThresholdCS(Map(), defaultBounds, Seq())
  def solved(s: CSubst) = new SolveContinuousSubstThresholdCS(s, defaultBounds, Seq())
  def notyet(c: Constraint) = freshConstraintSystem addNewConstraint (c)
  def never(c: Constraint) = new SolveContinuousSubstThresholdCS(Map(), defaultBounds, Seq(c))
  def system(substitution: CSubst, bounds: Map[CVar[Type], (LBound, UBound)], never: Seq[Constraint]) = new SolveContinuousSubstThresholdCS(substitution, bounds, never)
}

case class SolveContinuousSubstThresholdCS(substitution: CSubst, bounds: Map[CVar[Type], (LBound, UBound)], never: Seq[Constraint]) extends ConstraintSystem[SolveContinuousSubstThresholdCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  def state = SolveContinuousSubstThreshold.state
  def stats = SolveContinuousSubstThreshold.state.stats

  lazy val trigger = substitution.size >= SolveContinuousSubstThreshold.threshold

  def notyet = {
    var cons = Seq[Constraint]()
    for ((x, (l, u)) <- bounds) {
      val join = subtype.Join(UVar(x), l.nonground ++ l.ground.toSet)
      val meet = subtype.Meet(UVar(x), u.nonground ++ u.ground.toSet)
      cons = cons :+ join :+ meet
    }
    cons
  }

  def never(c: Constraint) = SolveContinuousSubstThresholdCS(substitution, bounds, never :+ c)

  def mergeSubsystem(that: SolveContinuousSubstThresholdCS) = {
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

    SolveContinuousSubstThresholdCS(msubst, mbounds, mnever)
  }

  def addNewConstraint(c: Constraint) = {
    stats.addToConstraintCount(1)
    stats.constraintSolveTimed {
      c.solve(this).trySolve
    }
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    stats.addToConstraintCount(cons.size)
    stats.constraintSolveTimed {
      cons.foldLeft(this)((cs, c) => c.solve(cs)).trySolve
    }
  }

  def tryFinalize = {
    SolveContinuously.state = state
    SolveContinuouslyCS(Map(), bounds, never).tryFinalize
  }


  private def substitutedBounds(s: CSubst) = {
    var newnever = Seq[Constraint]()
    val newbounds: Map[CVar[Type], (LBound,UBound)] = for ((tv, (lb, ub)) <- bounds) yield {
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
    SolveContinuousSubstThresholdCS(substitution, newbounds, never ++ newnever)
  }

  def trySolve = saturateSolution

  private def saturateSolution = {
    var current = this.withSubstitutedBounds
    var sol = solveOnce
    while (sol.nonEmpty) {
      val subst = substitution ++ sol
      val (newbounds, newnever) = current.substitutedBounds(sol)

      var temp = SolveContinuousSubstThresholdCS(subst, SolveContinuousSubstThreshold.defaultBounds, Seq())

      for ((tv, (lb, ub)) <- newbounds) {
        val t = subst.hgetOrElse(tv, UVar(tv))
        for(tpe <- lb.ground.toSet ++ lb.nonground)
          temp = tpe.subtype(t, temp)
        for(tpe <- ub.ground.toSet ++ ub.nonground)
          temp = t.subtype(tpe, temp)
      }

      current = SolveContinuousSubstThresholdCS(temp.substitution, temp.bounds, current.never ++ newnever ++ temp.never)
      sol = current.solveOnce
    }
    current
  }

  private def solveOnce: CSubst = {
    var sol = CSubst.empty
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

  def addLowerBound(v: CVar[Type], t: Type) = {
    val (lower, upper) = bounds(v)
    val (newLower, error) = lower.add(t)
    val changed = if (newLower.isGround) newLower.ground.get else t

    val newnever =
      if (error.isEmpty)
        never
      else
        never :+ subtype.Join(UVar(v), error)
    val newbounds = bounds + (v -> (newLower, upper))
    val cs = SolveContinuousSubstThresholdCS(substitution, newbounds, newnever)

    subtype.Meet(changed, upper.nonground ++ upper.ground.toSet).solve(cs)
  }

  def addUpperBound(v: CVar[Type], t: Type) = {
    val (lower, upper) = bounds(v)
    val (newUpper, error) = upper.add(t)
    val changed = if (newUpper.isGround) newUpper.ground.get else t

    val newnever =
      if (error.isEmpty)
        never
      else
        never :+ subtype.Meet(UVar(v), error)
    val newbounds = bounds + (v -> (lower, newUpper))
    val cs = SolveContinuousSubstThresholdCS(substitution, newbounds, newnever)

    subtype.Join(changed, lower.nonground ++ lower.ground.toSet).solve(cs)
  }


  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) =
    if (trigger)
      t.subst(substitution)
    else
      t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = if (trigger)
      it.map(u => (u, f(u).subst(substitution)))
    else
      it

  def propagate =
    if (trigger)
      SolveContinuousSubstThresholdCS(Map(), bounds, never)
    else
      this
}