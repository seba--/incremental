package constraints.equality_letpoly.impl

import constraints.{CVar, Statistics}
import constraints.equality_letpoly._
import constraints.equality_letpoly.CSubst.CSubst
import incremental.Util
import incremental.pcf.let_poly.TFloat

import scala.collection.generic.CanBuildFrom

object SolveContinuously extends ConstraintSystemFactory[SolveContinuouslyCS] {
  val freshConstraintSystem = SolveContinuouslyCS(CSubst.empty, Seq(), Seq(), Map())
}

case class SolveContinuouslyCS(substitution: CSubst, notyet: Seq[Constraint], never: Seq[Constraint], compatibleC : Map[Type, Set[Type]]) extends ConstraintSystem[SolveContinuouslyCS] {
  def state = SolveContinuously.state.value

  def solved(s: CSubst) = {
    var current = SolveContinuouslyCS(substitution mapValues (x => x.subst(s)), notyet, never, compatibleC)
    for ((x, t2) <- s) {
      current.substitution.get(x) match {
        case None => current = SolveContinuouslyCS(current.substitution + (x -> t2.subst(current.substitution)), current.notyet, current.never, current.compatibleC)
        case Some(t1) => current = t1.compatibleWith(t2).solve(current)
      }
    }
    current
  }

  def notyet(c: Constraint) = SolveContinuouslyCS(substitution, notyet :+ c, never, compatibleC)

  def never(c: Constraint) = SolveContinuouslyCS(substitution, notyet, never :+ c, compatibleC)

  def without(xs: Set[CVar[_]]) = SolveContinuouslyCS(substitution -- xs, notyet, never, compatibleC)

  def mergeSubsystem(other: SolveContinuouslyCS): SolveContinuouslyCS =
    Util.timed(state -> Statistics.mergeSolutionTime) {
      val msubstitution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      val init = SolveContinuousSubstCS(msubstitution, mnotyet, mnever, this.compatibleC)
      val mcompCS = other.compatibleC.foldLeft(init) { case (cs, (t1, sett2)) => sett2.toSeq.foldLeft(init) { case (_, t2) => cs.addcompatibleCons(t1, t2) } } //for (i <- 0 until sett2.toSeq.size) cs.addcompatibleCons(t1, sett2.toSeq(i)) }
      //      var mcompatibleC = compatibleC
      //      for ((x, typ ) <- other.compatibleC) {
      //        compatibleC.get(x) match {
      //          case None => mcompatibleC += x -> typ
      //          case Some(typ2) => mcompatibleC += x -> (typ ++ typ2)
      //        }
      //      }
      SolveContinuouslyCS(msubstitution, mnotyet, mnever, mcompCS.compatibleC)
    }

  def addNewConstraint(c: Constraint) = {
    state += Statistics.constraintCount -> 1
    Util.timed(state -> Statistics.constraintSolveTime) {
      c.solve(this)
    }
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state += Statistics.constraintCount -> cons.size
    Util.timed(state -> Statistics.constraintSolveTime) {
      cons.foldLeft(this)((cs, c) => c.solve(cs))
    }
  }

  def shouldApplySubst: Boolean = false

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
  (it: C, f: U => CT)
  (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = it

  def propagate = this

  def tryFinalize: SolveContinuouslyCS =
    Util.timed(state -> Statistics.finalizeTime) {
      trySolve(true)
    }

//  def tryFinalize: SolveContinuouslyCS = {
//    Util.timed(state -> Statistics.finalizeTime) {
//      tryFinalize(0) //trySolve(True)
//    }
//  }
//
//  def tryFinalize(steps: Int): SolveContinuouslyCS = {
//    var newcons = Seq[Constraint]()
//    for ((t1, t1Set) <- compatibleC) {
//      if (t1.subst(substitution).isGround) {
//        for (i <- 0 until t1Set.size)
//          newcons = newcons :+ EqConstraint(t1.subst(substitution), t1Set.toSeq(i).subst(substitution))
//      }
//      else
//        newcons
//    }
//    val newcs = notyet.foldLeft(copy(substitution =substitution, notyet = Seq(), never = never,  compatibleC = compatibleC))((cs, cons) => cons.solve(cs))
//
//    val startSize = notyet.size
//    val endSize = newcs.notyet.size
//    if (endSize > 0 && endSize < startSize)
//      newcs.tryFinalize(steps + 1)
//    else
//      newcs
//  }

  def addcompatibleCons(t1: Type, t2: Type) = {
    var cons = Seq[Constraint]()
    var current = this
    val t1p = t1.subst(substitution)
    compatibleC.get(t1) match {
      case None => current = this.copy(current.substitution, current.notyet, current.never, current.compatibleC + (t1 -> Set(t2)))
      case Some(s2) => current = this.copy(current.substitution, current.notyet, current.never, current.compatibleC + (t1 -> (s2 + t2)))
    }
    if (t1p.isGround) {
      current.compatibleC.get(t1) match {
        case None => current
        case Some(typ) =>
          for (i <- 0 until typ.size)
            cons = cons :+  EqConstraint(t1p, typ.toSeq(i).subst(substitution))
          current = this.copy(current.substitution, current.notyet ++ cons, current.never, current.compatibleC)
      }
      current
    }
    else
      current
  }

//  def checkCompCons  = {
//    var cons = Seq[Constraint]()
//    var current = this
//    for ((t, comS) <- current.compatibleC) {
//      if (t.isGround) {
//        for (i <- 0 until comS.size)
//          cons = cons :+  EqConstraint(t, comS.toSeq(i).subst(substitution))
//        current = this.copy(current.substitution, current.notyet ++ cons, current.never, current.compatibleC)
//      }
//      else current
//    }
//    current
//  }


  private def trySolve(finalize: Boolean): SolveContinuouslyCS = {
      var current = this
      var stepsWithoutChange = 0
      while (!current.notyet.isEmpty) {
        val next = current.notyet.head
        val rest = current.notyet.tail
        current = SolveContinuouslyCS(current.substitution, rest, current.never, current.compatibleC)
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