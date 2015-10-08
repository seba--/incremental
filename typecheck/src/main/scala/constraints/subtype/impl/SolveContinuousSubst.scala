package constraints.subtype.impl

import constraints.subtype.CSubst.CSubst
import constraints.{Statistics, CVar, subtype}
import constraints.subtype._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  def freshConstraintSystem = new SolveContinuousSubstCS(Map(), defaultBounds, Seq(), Map())
  def solved(s: CSubst) = new SolveContinuousSubstCS(s, defaultBounds, Seq(), Map())
  def notyet(c: Constraint) = freshConstraintSystem addNewConstraint (c)
  def never(c: Constraint) = new SolveContinuousSubstCS(Map(), defaultBounds, Seq(c), Map())
}

case class SolveContinuousSubstCS(substitution: CSubst, bounds: Map[CVar[Type], (LBound, UBound)], never: Seq[Constraint], extend: Map[Type, Type]) extends ConstraintSystem[SolveContinuousSubstCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  def state = SolveContinuousSubst.state.value

  def notyet = {
    var cons = Seq[Constraint]()
    for ((x, (l, u)) <- bounds) {
      val join = subtype.Join(UCName(x), l.nonground ++ l.ground.toSet)
      val meet = subtype.Meet(UCName(x), u.nonground ++ u.ground.toSet)
      cons = cons :+ join :+ meet
    }
    cons
  }

  def never(c: Constraint) = SolveContinuousSubstCS(substitution, bounds, never :+ c, extend)

  def mergeSubsystem(that: SolveContinuousSubstCS) = {
    val msubst = substitution ++ that.substitution
    val mextend = extend ++ that.extend
    var mbounds = bounds
    var mnever = never ++ that.never

    for((tv, (l1, u1)) <- that.bounds) {
      val (l2, u2) = mbounds(tv)
      val (newL, errorl) = l2 merge l1
      val (newU, erroru) = u2 merge u1
      if(errorl.nonEmpty)
        mnever = mnever :+ subtype.Join(UCName(tv), errorl)
      if(erroru.nonEmpty)
        mnever = mnever :+ subtype.Meet(UCName(tv), erroru)
      val merged = (newL, newU)
      mbounds = mbounds + (tv -> merged)
    }

    SolveContinuousSubstCS(msubst, mbounds, mnever, mextend)
  }

  def addNewConstraint(c: Constraint) = {
    state += Statistics.constraintCount -> 1
    Util.timed(state -> Statistics.constraintSolveTime) {
      c.solve(this).trySolve
    }
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state += Statistics.constraintCount -> cons.size
    Util.timed(state -> Statistics.constraintSolveTime) {
      cons.foldLeft(this)((cs, c) => c.solve(cs)).trySolve
    }
  }

  def tryFinalize =
    Util.timed(state -> Statistics.finalizeTime) {
      //set upper bounds of negative vars to Top if still undetermined and solve
      val finalbounds = bounds.map {
        case (tv, (lower, upper)) if gen.isNegative(tv) && !upper.isGround =>
          val (newUpper, _) = upper.add(subtype.Top)
          (tv, (lower, newUpper))
        case x => x
      }

      SolveContinuousSubstCS(substitution, finalbounds, never, extend).saturateSolution
    }


  private def substitutedBounds(s: CSubst) = {
    var newnever = Seq[Constraint]()
    val newbounds: Map[CVar[Type], (LBound,UBound)] = for ((tv, (lb, ub)) <- bounds) yield {
      val (newLb, errorl) = lb.subst(s)
      val (newUb, erroru) = ub.subst(s)
      if(errorl.nonEmpty)
        newnever  = newnever  :+ subtype.Join(UCName(tv).subst(s), errorl)
      if(erroru.nonEmpty)
        newnever  = newnever  :+ subtype.Meet(UCName(tv).subst(s), erroru)
      (tv -> (newLb, newUb))
    }
    (newbounds, newnever)
  }

  private def withSubstitutedBounds = {
    val (newbounds, newnever) = substitutedBounds(substitution)
    SolveContinuousSubstCS(substitution, newbounds, never ++ newnever, extend)
  }

  def trySolve = saturateSolution

  private def saturateSolution = {
    var current = this.withSubstitutedBounds
    var sol = solveOnce
    while (sol.nonEmpty) {
      val subst = substitution ++ sol
      val (newbounds, newnever) = current.substitutedBounds(sol)

      var temp = SolveContinuousSubstCS(subst, SolveContinuousSubst.defaultBounds, Seq(), extend)

      for ((tv, (lb, ub)) <- newbounds) {
        val t = subst.hgetOrElse(tv, UCName(tv))
        for(tpe <- lb.ground.toSet ++ lb.nonground)
          temp = tpe.subtype(t, temp)
        for(tpe <- ub.ground.toSet ++ ub.nonground)
          temp = t.subtype(tpe, temp)
      }

      current = SolveContinuousSubstCS(temp.substitution, temp.bounds, current.never ++ newnever ++ temp.never, extend ++ temp.extend)
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
        never :+ subtype.Join(UCName(v), error)
    val newbounds = bounds + (v -> (newLower, upper))
    val cs = SolveContinuousSubstCS(substitution, newbounds, newnever, extend)

    subtype.Meet(changed, upper.nonground ++ upper.ground.toSet).solve(cs)

  }

  def addExtend(t1 : Type, t2:Type) = {
    var mextend = extend// ++ Map(t1 -> t2)
   if (extend.exists(t => t._1 == t1 && t._2 == t2)) mextend
    else  mextend = mextend + (t1-> t2)

     val cs =  SolveContinuousSubstCS(substitution, bounds, never, mextend)
    subtype.Subtype(t1, t2).solve(cs)

  }

  def findM(t1 : Type, t2 : Type, extend: Map[Type, Type]): Boolean
  = { println(s"findM($t1, $t2, $extend)");
    extend.get(t1) match {
    case None => false
    case Some(u) =>
      if (u == t2) true
      else findM(u, t2, extend)

  }
  }

  def addUpperBound(v: CVar[Type], t: Type) = {
    val (lower, upper) = bounds(v)
    val (newUpper, error) = upper.add(t)
    val changed = if (newUpper.isGround) newUpper.ground.get else t

    val newnever =
      if (error.isEmpty)
        never
      else
        never :+ subtype.Meet(UCName(v), error)
    val newbounds = bounds + (v -> (lower, newUpper))
    val cs = SolveContinuousSubstCS(substitution, newbounds, newnever, extend)

    subtype.Join(changed, lower.nonground ++ lower.ground.toSet).solve(cs)
  }


  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = it.map(u => (u, f(u).subst(substitution)))

  def propagate = SolveContinuousSubstCS(Map(), bounds, never, extend)
}