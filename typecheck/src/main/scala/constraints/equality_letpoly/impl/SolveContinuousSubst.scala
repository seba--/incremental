package constraints.equality_letpoly.impl

import constraints.{CSubst, CTermBase, CVar, Statistics}
import constraints.equality_letpoly._
import constraints.equality_letpoly.CSubst.CSubst
import incremental.Util
import incremental.pcf.let_poly.TFloat

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] {
  val freshConstraintSystem = SolveContinuousSubstCS(CSubst.empty, Seq(), Seq(), Map())
}

case class SolveContinuousSubstCS(substitution: CSubst, notyet: Seq[Constraint], never: Seq[Constraint], compatibleC : Map[Type, Set[Type]]) extends ConstraintSystem[SolveContinuousSubstCS] {
  def state = SolveContinuousSubst.state.value

  def solved(s: CSubst) = {
    var current = SolveContinuousSubstCS(substitution mapValues (x => x.subst(s)), notyet, never, compatibleC)
    for ((x, t2) <- s) {
      current.substitution.get(x) match {
        case None => current = SolveContinuousSubstCS(current.substitution + (x -> t2.subst(current.substitution)), current.notyet, current.never, current.compatibleC)
        case Some(t1) => current = t1.compatibleWith(t2).solve(current)
      }
    }
    current
  }


  def notyet(c: Constraint) = SolveContinuousSubstCS(substitution, notyet :+ c, never, compatibleC)
  def never(c: Constraint) = SolveContinuousSubstCS(substitution, notyet, never :+ c, compatibleC)
  def without(xs: Set[CVar[_]]) = SolveContinuousSubstCS(substitution -- xs, notyet, never, compatibleC)

  def mergeSubsystem(other: SolveContinuousSubstCS): SolveContinuousSubstCS =
    Util.timed(state -> Statistics.mergeSolutionTime) {
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      val init = SolveContinuousSubstCS(CSubst.empty, mnotyet, mnever, this.compatibleC)
      val mcompCS = other.compatibleC.foldLeft(init) { case (cs, (t1, sett2)) => sett2.toSeq.foldLeft(init) { case (_, t2) =>  cs.addcompatibleCons(t1,t2) } } //for (i <- 0 until sett2.toSeq.size) cs.addcompatibleCons(t1, sett2.toSeq(i)) }
        //      var mcompatibleC = compatibleC
//      for ((x, typ ) <- other.compatibleC) {
//        compatibleC.get(x) match {
//          case None => mcompatibleC += x -> typ
//          case Some(typ2) => mcompatibleC += x -> (typ ++ typ2)
//        }
//      }
    SolveContinuousSubstCS(CSubst.empty, mnotyet, mnever, mcompCS.compatibleC)
    }

  def addcompatibleCons(t1 : Type, t2 : Type) = {
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

  def shouldApplySubst: Boolean = true
  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C]): C
  = it.map(u => (u, f(u).subst(substitution)))

  def propagate = SolveContinuousSubstCS(CSubst.empty, notyet.map(_.subst(substitution)), never.map(_.subst(substitution)), compatibleC.map(f => f._1.subst(substitution) -> f._2.map(_.subst(substitution))))

  override def tryFinalize =
    SolveContinuously.state.withValue(state) {
      SolveContinuouslyCS(substitution, notyet, never, compatibleC).tryFinalize
    }
}