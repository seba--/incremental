package constraints.fjava.impl


import constraints.{CTermBase, CVar, Statistics}
import constraints.fjava.CSubst.CSubst
import incremental.fjava.{UCName, CName}
import constraints.fjava._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveContinuousSubst extends ConstraintSystemFactory[SolveContinuousSubstCS] with Serializable {
  def freshConstraintSystem = SolveContinuousSubstCS(Map(), Map(), Seq(), Seq(), Map())
}

case class SolveContinuousSubstCS(substitution: CSubst, bounds: Map[Type, Set[Type]], _notyet: Seq[Constraint], never: Seq[Constraint], extend: Map[GroundType, GroundType]) extends ConstraintSystem[SolveContinuousSubstCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  def state = SolveContinuousSubst.state.value

  def notyet = {
    var cons = _notyet
    for ((t, tbnds) <- bounds; bound <- tbnds) {
      cons = Subtype(t, bound) +: cons
    }
    cons
  }

  def notyet(c: Constraint) = SolveContinuousSubstCS(substitution, bounds, c +: _notyet, never, extend)
  def never(c: Constraint) = SolveContinuousSubstCS(substitution, bounds, _notyet, c +: never, extend)

  def mergeSubsystem(that: SolveContinuousSubstCS) = {
    var msubst = substitution ++ that.substitution
    var mnotyet = _notyet ++ that.notyet
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
      for ((t, ts) <- this.bounds){
          lt = ts
          if (t.isGround && extend.contains(t.asInstanceOf[GroundType])) {
            ts.foreach { f =>
              if (isSubtype(t, f))
                lt = lt - f
              else lt
            }
          }
          if (lt.isEmpty) newBounds = newBounds
          else newBounds = newBounds + (t -> lt)
      }

      var current = SolveContinuousSubstCS(this.substitution, newBounds, this._notyet, this.never, this.extend)
      var stepsWithoutChange = 0
      while (!current._notyet.isEmpty) {
        val next = current._notyet.head
        val rest = current._notyet.tail
        current = SolveContinuousSubstCS(current.substitution, current.bounds, rest, current.never, current.extend)
        current = next.solve(current)

        if (current._notyet.size == rest.size + 1) {
          stepsWithoutChange += 1
          if (stepsWithoutChange > rest.size + 1)
            return current
        }
        else
          stepsWithoutChange = 0
      }
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
    if (t2 == CName('Object))
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


  def addUpperBound(t1: Type, t2: Type) =
    if (t1 == t2)
      this
    else if (isSubtype(t1, t2))
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

  def propagate =
    Util.timed(state -> Statistics.constraintSolveTime) {
      _notyet.foldLeft(copy(substitution = Map(), _notyet = Seq()))((cs, c) => c.solve(cs)).trySolve
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
    val init = SolveContinuousSubstCS(mysubst, Map(), this._notyet.map(_.subst(s)), this.never.map(_.subst(s)), this.extend).addNewConstraints(newcons)
    val cs = this.extend.foldLeft(init) { case (cs, (t, tsuper)) =>
      val t2 = t.subst(s)
      val tsuper2 = tsuper.subst(s)
      if (t2.isGround)
        cs.extend.get(t2.asInstanceOf[GroundType]) match {
          case Some(up) => Equal(tsuper, up).solve(cs)
          case None => cs
        }
      else
        cs
    }
    val extendedCS = bounds.foldLeft(cs) { case (cs, (t,ts)) =>
      ts.foldLeft(cs) { case (cs, tsuper) => cs.addUpperBound(t.subst(s), tsuper.subst(s))}
    }
    extendedCS
  }

}