package constraints.fjava.impl


import constraints.{CTermBase, CVar, Statistics}
import constraints.fjava.CSubst.CSubst
import incremental.fjava.{CName, UCName}
import constraints.fjava._
import incremental.Util
import incremental.fjava.earlymerge.{Condition, ConditionOther, Conditional}

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] with Serializable {
  def freshConstraintSystem = SolveContinuousSubstCS(Map(), Map(), Map(), Seq(), Map())
}

case class SolveContinuousSubstCS(substitution: CSubst, bounds: Map[Type, Set[Type]], _notyet: Map[ConditionOther, Seq[Constraint]], never: Seq[Constraint], extend: Map[GroundType, GroundType]) extends ConstraintSystem[SolveContinuousSubstCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  def state = SolveContinuousSubst.state.value

  def notyet = {
    var cons: Seq[Constraint] = _notyet.flatMap {
      case (cond, cons) => cons.map(Conditional(_, cond.cls, cond.cond))
    }.toSeq
    for ((t, tbnds) <- bounds; bound <- tbnds) {
      cons = Subtype(t, bound) +: cons
    }
    cons
  }

  def extendNotyet(cond: ConditionOther, c: Constraint, notyet: Map[ConditionOther, Seq[Constraint]]): Map[ConditionOther, Seq[Constraint]] =
    notyet.get(cond) match {
      case None => notyet + (cond -> Seq(c))
      case Some(seq) => notyet + (cond -> (c +: seq))
    }

  def extendNotyet(cond: ConditionOther, cons: Seq[Constraint], notyet: Map[ConditionOther, Seq[Constraint]]): Map[ConditionOther, Seq[Constraint]] =
    notyet.get(cond) match {
      case None => notyet + (cond -> cons)
      case Some(seq) => notyet + (cond -> (seq ++ cons))
    }

  def notyet(c: Constraint) = {
    val c_ = c.asInstanceOf[Conditional]
    val condOther =
      if (c_.cond.isInstanceOf[Condition])
        ConditionOther(c_.cls, c_.cond.asInstanceOf[Condition]).asInstanceOf[ConditionOther]
      else
        c_.cond.asInstanceOf[ConditionOther]
    val newNotyet = extendNotyet(condOther, c_.cons, _notyet)
    SolveContinuousSubstCS(substitution, bounds, newNotyet, never, extend)
  }
  def never(c: Constraint) = SolveContinuousSubstCS(substitution, bounds, _notyet, c +: never, extend)

  def mergeSubsystem(that: SolveContinuousSubstCS) = {
    var msubst = substitution ++ that.substitution
    var mnotyet = _notyet ++ that._notyet
    var mnever = never ++ that.never
    val init = SolveContinuousSubstCS(msubst, this.bounds, mnotyet, mnever, this.extend)
    val extendedCS = that.extend.foldLeft(init) { case (cs, (t1, t2)) => cs.extendz(t1, t2) }
    that.bounds.foldLeft(extendedCS) { case (cs, (t, ts)) =>
      ts.foldLeft(cs) { case (cs2, t2) => cs2.addUpperBound(t, t2)}
    }
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

  def tryFinalize: SolveContinuousSubstCS =
    Util.timed(state -> Statistics.finalizeTime) {
      var newBounds = Map[Type, Set[Type]]()
      var lt = Set[Type]()
      for ((low, ups) <- this.bounds){
        val newlow = low.subst(substitution)
        val newups = ups.map(_.subst(substitution))
        lt = newups
        if (newlow.isGround && extend.contains(newlow.asInstanceOf[GroundType])) {
          newups.foreach { f =>
            if (isSubtype(newlow, f))
              lt = lt - f
            else lt
          }
        }
        if (lt.isEmpty) newBounds = newBounds
        else newBounds = newBounds + (newlow -> lt)
      }

      var current = _notyet.foldLeft(copy(bounds = newBounds, _notyet = Map()))((cs, cons) =>
        cons._2.foldLeft(cs)((cs, c) => Conditional(c, cons._1.cls, cons._1.cond).solve(cs))
      )
//      var stepsWithoutChange = 0
//      while (!current._notyet.isEmpty) {
//        val next = current._notyet.head
//        val rest = current._notyet.tail
//        current = SolveContinuousSubstCS(current.substitution, current.bounds, rest, current.never, current.extend)
//        current = next.solve(current)
//
//        if (current._notyet.size == rest.size + 1) {
//          stepsWithoutChange += 1
//          if (stepsWithoutChange > rest.size + 1)
//            return current
//        }
//        else
//          stepsWithoutChange = 0
//      }
      current
    }

  def trySolve: SolveContinuousSubstCS = this

  def extendz(t1 : GroundType, t2:GroundType) = {
    if (t1 == t2 || isSubtype(t2, t1))
      never(Subtype(t1, t2))
    else {
      t1 match {
        case CName('Object) =>
          this.never(Subtype(t1, t2))
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

  private def extendMap(t1: GroundType, t2: GroundType) =
    SolveContinuousSubstCS(this.substitution, this.bounds, this._notyet, this.never, this.extend + (t1 -> t2))

  def isSubtype(t1 : Type, t2 : Type): Boolean =
    if (t1 == t2)
      true
    else if (t2 == CName('Object))
      true
    else if (t1 == CName('Object))
      false
    else if (t1.isGround)
      extend.get(t1.asInstanceOf[GroundType]) match {
        case None => false
        case Some(u) => u == t2 || isSubtype(u, t2)
      }
    else
      false


  def addUpperBound(t1: Type, t2: Type): SolveContinuousSubstCS =
    if (isSubtype(t1, t2))
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
    SolveContinuousSubstCS(this.substitution, bounds + (t1 -> (t1bnds + t2)), this._notyet, this.never, this.extend)
  }

  def shouldApplySubst: Boolean = true

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = it.map(u => (u, f(u).subst(substitution)))

  def propagate: SolveContinuousSubstCS = {
    if (substitution.isEmpty)
      return this

    var newBounds = Map[Type, Set[Type]]()
    bounds.foreach { case (low, ups) =>
      val newlow = low.subst(substitution)
      val newups = ups.flatMap { up =>
        val newup = up.subst(substitution)
        if (isSubtype(newlow, newup)) None
        else Some(newup)
      }
      if (newups.nonEmpty)
        newBounds.get(newlow) match {
          case None => newBounds += (newlow -> newups)
          case Some(currentUps) => newBounds += (newlow -> (currentUps ++ newups))
        }
    }
    val newNever = never.map(_.subst(substitution))

    var newNotyet = Map[ConditionOther, Seq[Constraint]]()
    var now = Seq[Constraint]()
    _notyet.foreach { case (cond, cons) =>
      cond.subst(null, substitution) match {
        case None => // condition failed, discard constraints
        case Some(cond_) =>
          if (Condition.trueCond == cond_)
            now ++= cons.map(_.subst(substitution))
          else
            newNotyet = extendNotyet(cond_.asInstanceOf[ConditionOther], cons.map(_.subst(substitution)), newNotyet)
      }
    }

    val cs = SolveContinuousSubstCS(Map(), newBounds, newNotyet, newNever, extend)
    if (now.isEmpty)
      cs
    else
      Util.timed(state -> Statistics.constraintSolveTime) {
        now.foldLeft(cs)((cs, c) => c.solve(cs))
      }
  }

  def solved(s: CSubst): SolveContinuousSubstCS = {
    var mysubst = this.substitution.mapValues(x => x.subst(s)).view.force
    var newcons = Seq[Constraint]()
    for ((x, t2) <- s) {
      mysubst.get(x) match {
        case None => mysubst = mysubst + (x -> t2.subst(mysubst))
        case Some(t1) => newcons = t1.compatibleWith(t2) +: newcons
      }
    }
    SolveContinuousSubstCS(mysubst, bounds, this._notyet, this.never, this.extend).addNewConstraints(newcons)
  }

}