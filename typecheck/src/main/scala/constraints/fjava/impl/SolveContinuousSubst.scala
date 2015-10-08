package constraints.fjava.impl


import constraints.{subtype, Statistics}
import constraints.fjava.CSubst.CSubst
import incremental.fjava.{UCName, CName}
import constraints.fjava._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  def freshConstraintSystem = new SolveContinuousSubstCS(Map(), Map(), Seq(), Map())
  def solved(s: CSubst) = new SolveContinuousSubstCS(s, Map(), Seq(), Map())
  def notyet(c: Constraint) = freshConstraintSystem.addNewConstraint(c)
  def never(c: Constraint) = new SolveContinuousSubstCS(Map(), Map(), Seq(c), Map())
}

case class SolveContinuousSubstCS(substitution: CSubst, bounds: Map[Type, Set[Type]], never: Seq[Constraint], extend: Map[Type, Type]) extends ConstraintSystem[SolveContinuousSubstCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  def state = SolveContinuousSubst.state.value

  def notyet = {
    var cons = Seq[Constraint]()
    for ((t, tbnds) <- bounds; bound <- tbnds) {
      cons = cons :+ Subtype(t, bound)
    }
    cons
  }

  def never(c: Constraint) = SolveContinuousSubstCS(substitution, bounds, never :+ c, extend)

  def mergeSubsystem(that: SolveContinuousSubstCS) = {
    val msubst = substitution ++ that.substitution
    /*val mextend = extend ++ that.extend
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

    SolveContinuousSubstCS(msubst, mbounds, mnever, mextend)*/
    ??? //TODO
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
      //set upper bounds of vars to Object if still undetermined and solve
     /* val finalbounds = bounds.map {
        case (tv, ) if gen.isNegative(tv) && !upper.isGround =>
          val (newUpper, _) = upper.add(subtype.Top)
          (tv, (lower, newUpper))
        case x => x
      }

      SolveContinuousSubstCS(substitution, finalbounds, never, extend).saturateSolution*/
      ???
    }

   def trySolve: SolveContinuousSubstCS = ???

 /* private def substitutedBounds(s: CSubst) = {
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
  }*/

  def addLowerBound(t1: Type, t2: Type) = addUpperBound(t2, t1)

  def extendz(t1 : Type, t2:Type) = {
    if (t1 == t2) never(Extend(t1, t2))
    else {
      t1 match {
        case CName('Object) =>
          this.never(Extend(t1, t2))
        case _ =>
          extend.get(t1) match {
            case None =>
              extendMap(t1, t2)
            case Some(t3) =>
              Equal(t2, t3).solve(this)
         }
      }
    }
  }

  private def extendMap(t1: Type, t2: Type) =
    SolveContinuousSubstCS(this.substitution, this.bounds, this.never, this.extend + (t1 -> t2))

  def findM(t1 : Type, t2 : Type, extend: Map[Type, Type]): Boolean = {
    (t1, t2) match {
      case (_, CName('Object)) => true
      case (CName('Object), _) => false
      case _ =>  extend.get(t1) match {
        case None => false
        case Some(u) =>
          if (u == t2) true
          else findM(u, t2, extend)

      }
    }
  }

  def addUpperBound(t1: Type, t2: Type) =
    if (t1 == t2)
      this
    else (t1, t2) match {
      case (CName('Object), UCName(x)) =>
        Equal(t1, t2).solve(this)
      case (CName('Object), CName(n)) if n != 'Object =>
        this.never(Subtype(t1, t2))
      case _  =>
        extendBound(t1, t2)
    }


  private def extendBound(t1: Type, t2: Type) = {
    val t1bnds = bounds.getOrElse(t1, Set[Type]())
    SolveContinuousSubstCS(this.substitution, bounds + (t1 -> (t1bnds + t2)), this.never, this.extend)
  }

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = it.map(u => (u, f(u).subst(substitution)))

  def propagate = SolveContinuousSubstCS(Map(), bounds, never, extend)


  def solved(s: CSubst): SolveContinuousSubstCS = ???
}