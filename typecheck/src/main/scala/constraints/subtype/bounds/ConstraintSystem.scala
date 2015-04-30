package constraints.subtype.bounds

import constraints.subtype
import constraints.subtype.{Constraint, Type, UVar}
import constraints.subtype.Type.Companion._
import incremental.Util

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]](val substitution: TSubst, val bounds: Map[Symbol, (LBound, UBound)], val never: Seq[Constraint]) extends constraints.subtype.ConstraintSystem[CS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  implicit val csf: ConstraintSystemFactory[CS]
  import csf.state
  import csf.gen
  import csf.system

  def notyet = {
    var cons = Seq[Constraint]()
    for ((x, (l, u)) <- bounds) {
      val join = subtype.Join(UVar(x), l.nonground ++ l.ground.toSet)
      val meet = subtype.Meet(UVar(x), u.nonground ++ u.ground.toSet)
      cons = cons :+ join :+ meet
    }
    cons
  }

  def mergeSubsystem(that: CS) = {
    var current = system(substitution ++ that.substitution, bounds, never ++ that.never)

    for((tv, (thatL, thatU)) <- that.bounds) {
      if (bounds.isDefinedAt(tv)) {
        for (t <- thatL.nonground ++ thatL.ground.toSet)
          current = current.addLowerBound(tv, t)
        for (t <- thatU.nonground ++ thatU.ground.toSet)
          current = current.addUpperBound(tv, t)
      }
      else
        current = system(current.substitution, current.bounds + (tv -> (thatL, thatU)), current.never)
    }

    current
  }

  /* //add and solve immediately
   def <--(c: Constraint): ConstraintSystem = {
     val res = copy
     c match {
       case Equal(t1, t2) =>
         res.normalizeSub(t1, t2)
         res.normalizeSub(t2, t1)
         res.saturateSolution()
       case Subtype(lower, upper) =>
         res.normalizeSub(lower, upper)
         res.saturateSolution()
       case _ =>
     }
     res
   }*/

  def addNewConstraint(c: Constraint) = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this mergeSubsystem c.solve(this))
    state.value.stats.constraintSolveTime += time
    res
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state.value.stats.constraintCount += cons.size
    val (res, time) = Util.timed {
      cons.foldLeft(this.asCS)((cs, c) => cs mergeSubsystem c.solve(cs))
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

      system(substitution, finalbounds, never).saturateSolution
    }
    state.value.stats.finalizeTime += time
    res
  }



  def trySolve = saturateSolution

  private[ConstraintSystem] def saturateSolution = {
    var current = this.asCS
    var sol = solveOnce
    while (sol.nonEmpty) {
      var newnever = Seq[Constraint]()
      var temp = csf.freshConstraintSystem
      for ((tv, (lb, ub)) <- current.bounds) {
        val ((newLb, errorl), (newUb, erroru)) = (lb.subst(sol), ub.subst(sol))

        if(errorl.nonEmpty)
          newnever  = newnever  :+ subtype.Join(UVar(tv).subst(sol), errorl)
        if(erroru.nonEmpty)
          newnever  = newnever  :+ subtype.Meet(UVar(tv).subst(sol), erroru)

        val t = sol.getOrElse(tv, UVar(tv))

        for(tpe <- newLb.ground.toSet ++ newLb.nonground)
          temp = temp mergeSubsystem tpe.subtype(t, sol)
        for(tpe <- newUb.ground.toSet ++ newUb.nonground)
          temp = temp mergeSubsystem t.subtype(tpe, sol)
      }
      current = system(current.substitution ++ sol, temp.bounds, current.never ++ newnever ++ temp.never)
      sol = current.solveOnce
    }
    current
  }

  private[ConstraintSystem] def solveOnce: TSubst = {
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
    val cs = system(substitution, newbounds, newnever)

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
    val cs = system(substitution, newbounds, newnever)

    cs mergeSubsystem (subtype.Join(changed, lower.nonground ++ lower.ground.toSet).solve(cs))
  }


  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]](it: C, f: U=>Type)(implicit bf: CanBuildFrom[Iterable[U], (U, Type), C])
  = it

  def propagate = this.asCS

  private def asCS = system(substitution, bounds, never)

//  private[ConstraintSystem] def mergeSubsts(sigma: TSubst, tau: TSubst): TSubst = {
//    for ((v, t1) <- sigma if tau.isDefinedAt(v) && t1 != tau(v))
//      gameOver(Equal(t1, tau(v)))
//    tau ++ sigma
//  }
//
//  private[ConstraintSystem] def mergeSubsts(ss: Set[TSubst]): TSubst =
//    ss.fold(Map[Symbol, Type]()) { case (s, s1) => mergeSubsts(s, s1) }
}