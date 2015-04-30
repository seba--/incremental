package constraints.subtype.bounds

import constraints.subtype
import constraints.subtype.{Constraint, Type, UVar}
import constraints.subtype.Type.Companion._
import incremental.Util

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

case class ConstraintSystem(substitution: TSubst, bounds: Map[Symbol, (LBound, UBound)], never: Seq[Constraint]) extends constraints.subtype.ConstraintSystem[ConstraintSystem] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  import ConstraintSystemFactory.state
  import ConstraintSystemFactory.gen

  private implicit val csf = ConstraintSystemFactory

  def notyet = {
    var cons = Seq[Constraint]()
    for ((x, (l, u)) <- bounds) {
      val join = subtype.Join(UVar(x), l.nonground ++ l.ground.toSet)
      val meet = subtype.Meet(UVar(x), u.nonground ++ u.ground.toSet)
      cons = cons :+ join :+ meet
    }
    cons
  }

  def mergeSubsystem(that: ConstraintSystem): ConstraintSystem = {
    val msubst = substitution ++ that.substitution
    var mbounds = bounds
    var mnever = never ++ that.never

    for((tv, (thatL, thatU)) <- that.bounds) {
      val (thisL, thisU) = mbounds(tv)

      var newL: LBound = thisL
      var errorL = Set[Type]()
      for (t <- thatL.nonground ++ thatL.ground.toSet) {
        val (l,err) = newL add t
        newL = l
        errorL = errorL ++ err
      }
      if(errorL.nonEmpty)
        mnever = mnever :+ subtype.Join(UVar(tv), errorL)

      var newU: UBound = thisU
      var errorU = Set[Type]()
      for (t <- thatU.nonground ++ thatU.ground.toSet) {
        val (u,err) = newU add t
        newU = u
        errorU = errorU ++ err
      }
      if(errorU.nonEmpty)
        mnever = mnever :+ subtype.Meet(UVar(tv), errorU)

      val merged = (newL, newU)
      mbounds = mbounds + (tv -> merged)
    }
    ConstraintSystem(msubst, mbounds, mnever)
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
      cons.foldLeft(this)((cs, c) => cs mergeSubsystem c.solve(cs))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def tryFinalize: ConstraintSystem = {
    val (res, time) = Util.timed {
      //set upper bounds of negative vars to Top if still undetermined and solve
      val finalbounds = bounds.map {
        case (tv, (lower, upper)) if gen.isNegative(tv) && !upper.isGround =>
          val (newUpper, _) = upper.add(subtype.Top)
          (tv, (lower, newUpper))
        case x => x
      }

      ConstraintSystem(substitution, finalbounds, never).saturateSolution
    }
    state.value.stats.finalizeTime += time
    res
  }



  def trySolve = saturateSolution

  private[ConstraintSystem] def saturateSolution: ConstraintSystem = {
    var current = this
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
      current = ConstraintSystem(current.substitution ++ sol, temp.bounds, current.never ++ newnever ++ temp.never)
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

  def addLowerBound(v: Symbol, t: Type): ConstraintSystem = {
    val (lower, upper) = bounds(v)
    val (newLower, error) = lower.add(t)
    val changed = if (t.isGround) newLower.ground.get else t

    val newnever =
      if (error.isEmpty)
        never
      else
        never :+ subtype.Join(UVar(v), error)
    val newbounds = bounds + (v -> (newLower, upper))
    val cs = ConstraintSystem(substitution, newbounds, newnever)

    cs mergeSubsystem (subtype.Meet(changed, lower.nonground ++ lower.ground.toSet).solve(cs))
    (upper.nonground ++ upper.ground.toSet).foldLeft(cs)((cs, t2) => cs mergeSubsystem (changed.subtype(t2, substitution)))
  }

  def addUpperBound(v: Symbol, t: Type): ConstraintSystem = {
    val (lower, upper) = bounds(v)
    val (newUpper, error) = upper.add(t)
    val changed = if (t.isGround) newUpper.ground.get else t

    val newnever =
      if (error.isEmpty)
        never
      else
        never :+ subtype.Meet(UVar(v), error)
    val newbounds = bounds + (v -> (lower, newUpper))
    val cs = ConstraintSystem(substitution, newbounds, newnever)

    cs mergeSubsystem (subtype.Join(changed, lower.nonground ++ lower.ground.toSet).solve(cs))
//    (lower.nonground ++ lower.ground.toSet).foldLeft(cs)((cs, t2) => cs.normalizeSub(t2, changed))
  }


  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]](it: C, f: U=>Type)(implicit bf: CanBuildFrom[Iterable[U], (U, Type), C])
  = it

  def propagate = this

//  private[ConstraintSystem] def mergeSubsts(sigma: TSubst, tau: TSubst): TSubst = {
//    for ((v, t1) <- sigma if tau.isDefinedAt(v) && t1 != tau(v))
//      gameOver(Equal(t1, tau(v)))
//    tau ++ sigma
//  }
//
//  private[ConstraintSystem] def mergeSubsts(ss: Set[TSubst]): TSubst =
//    ss.fold(Map[Symbol, Type]()) { case (s, s1) => mergeSubsts(s, s1) }
}