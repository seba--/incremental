package constraints.fjava.impl


import constraints.Statistics
import constraints.fjava.CSubst.CSubst
import incremental.fjava.{UCName, CName}
import constraints.fjava._
import incremental.Util

import scala.annotation.tailrec
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
    var msubst = substitution ++ that.substitution
    var mnever = never ++ that.never
    val init = SolveContinuousSubstCS(msubst, this.bounds, mnever, this.extend)
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

  def tryFinalize =
    Util.timed(state -> Statistics.finalizeTime) {
      var newBounds = Map[Type, Set[Type]]()
      var lt = Set[Type]()
      for ((t, ts) <- this.bounds){
        if (t.isInstanceOf[UCName]) newBounds = newBounds
        else {
          lt = ts
          if (extend.contains(t)) {
            ts.foreach { f =>
              if (isSubtype(t, f))
                lt = lt - f
              else if (f.isInstanceOf[UCName]) lt = lt - f
              else lt
            }
          }
          if (lt.isEmpty) newBounds = newBounds
          else newBounds = newBounds + (t -> lt)
        }

      }

      SolveContinuousSubstCS(this.substitution, newBounds, this.never, this.extend )

    }

   def trySolve: SolveContinuousSubstCS = this

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

  def isSubtype(t1 : Type, t2 : Type): Boolean = {
    (t1, t2) match {
      case (_, CName('Object)) => true
      case (CName('Object), _) => false
      case _ =>  extend.get(t1) match {
        case None => false
        case Some(u) =>
          if (u == t2) true
          else isSubtype(u, t2)

      }
    }
  }

  def prefixMatch(prefix: List[Type], list: List[Type]): Boolean =
    prefix.length <= list.length && prefix.zip(list).forall {case (t1, t2) => t1 == t2}

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
    SolveContinuousSubstCS(this.substitution, bounds + (t1 -> (t1bnds + t2)), this.never, this.extend)
  }

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t.subst(substitution)

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = it.map(u => (u, f(u).subst(substitution)))

  def propagate = SolveContinuousSubstCS(Map(), bounds, never, extend)

  def solved(s: CSubst): SolveContinuousSubstCS = {
    val init = SolveContinuousSubstCS(this.substitution ++ s,Map(), this.never.map(_.subst(s)), Map())
    val cs = this.extend.foldLeft(init) { case (cs, (t, tsuper)) =>
      val t2 = t.subst(s)
      val tsuper2 = tsuper.subst(s)
      if (cs.extend.contains(t2))
        Equal(tsuper, cs.extend(t2)).solve(cs)
      else cs.extendz(t2, tsuper2)
    }
    val extendedCS = bounds.foldLeft(cs) { case (cs, (t,ts)) =>
      ts.foldLeft(cs) { case (cs, tsuper) => cs.addUpperBound(t.subst(s), tsuper.subst(s))}
    }
    println(s"$substitution")
    println(s"$cs")

    extendedCS
  }
}