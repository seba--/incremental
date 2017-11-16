package constraints.fjavaMO.impl

import constraints.{CTermBase, CVar, Statistics}
import constraints.fjavaMO.CSubst.CSubst
import incremental.fjavaMO.{UCName, CName}
import constraints.fjavaMO._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEnd extends ConstraintSystemFactory[SolveEndCS] with Serializable {
  def freshConstraintSystem = SolveEndCS(Seq(), Seq(), Map())
}

case class SolveEndCS(notyet: Seq[Constraint], never: Seq[Constraint], extend: Map[GroundType, GroundType]) extends ConstraintSystem[SolveEndCS] {
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  def state = SolveEnd.state.value

  def substitution = CSubst.empty

  def solved(s: CSubst) = throw new UnsupportedOperationException(s"SolveEnd cannot handle substitution $s")
  def notyet(c: Constraint) = SolveEndCS(c +: notyet, never, extend)
  def never(c: Constraint) = SolveEndCS(notyet, c +: never, extend)

  def mergeSubsystem(that: SolveEndCS) =
    Util.timed(state -> Statistics.mergeSolutionTime) {
      val mnotyet = notyet ++ that.notyet
      val mnever = never ++ that.never
      val init = SolveEndCS(mnotyet, mnever, this.extend)
      that.extend.foldLeft(init) { case (cs, (t1, t2)) => cs.extendz(t1, t2) }
    }

  def addNewConstraint(c: Constraint) = {
    state += Statistics.constraintCount -> 1
    Util.timed(state -> Statistics.constraintSolveTime) {
      SolveEndCS(c +: notyet, never, extend)
    }
  }

  def addNewConstraints(cons: Iterable[Constraint]) = {
    state += Statistics.constraintCount -> cons.size
    Util.timed(state -> Statistics.constraintSolveTime) {
      SolveEndCS(notyet ++ cons, never, extend)
    }
  }
//TODO Lira check this again if for minsel should be empty map or smth else
  override def tryFinalize =
    SolveContinuousSubstLateMerge.state.withValue(state) {
      val cs = notyet.foldLeft(SolveContinuousSubstCSLateMerge(Map(), Map(), Seq(), never, extend, Map() ))((cs, c) => c.solve(cs)).trySolve
      cs.tryFinalize
    }

  def trySolve: SolveEndCS = this

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
    SolveEndCS(notyet, never, extend + (t1 -> t2))

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

  def isCompleteH(cls :GroundType) : Boolean = {
    var sType = this.extend.getOrElse(cls, CName("OObject"))
    if (sType == CName("OObject")) false
    else if (sType == CName("Object")) true
    else  isCompleteH(sType)
  }

  def isAllSubtype(params: Seq[Type], bound: Seq[Type]): Boolean = {
    var res = true
    for (i <- 0 until params.length)
      if (!isSubtype(bound(i), bound(i)))
        res = false
    res
  }

  def addUpperBound(t1: Type, t2: Type) = throw new UnsupportedOperationException(s"SolveEnd cannot handle new bounds $t1<:$t2")

  //TODO lira see this again
  def addMinSel(cvar: Seq[Type], seqT: Seq[Type]) = throw new UnsupportedOperationException(s"Ambiguities found")

  def shouldApplySubst: Boolean = false

  def applyPartialSolution[CT <: constraints.CTerm[Gen, Constraint, CT]](t: CT) = t

  def applyPartialSolutionIt[U, C <: Iterable[U], CT <: constraints.CTerm[Gen, Constraint, CT]]
    (it: C, f: U=>CT)
    (implicit bf: CanBuildFrom[Iterable[U], (U, CT), C])
  = it

  def propagate = this

}