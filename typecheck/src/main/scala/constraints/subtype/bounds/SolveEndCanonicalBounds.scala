package constraints.subtype.bounds

import constraints.subtype
import constraints.subtype.{ConstraintSystem, Type, UVar, Constraint}
import constraints.subtype.Type.Companion._
import incremental.Util

import scala.collection.generic.CanBuildFrom

object SolveEndCanonicalBounds extends ConstraintSystemFactory[SolveEndCanonicalBoundsCS] {
  def freshConstraintSystem = SolveEndCanonicalBoundsCS(defaultBounds, Seq())
  def solved(s: TSubst) = throw new UnsupportedOperationException(s"SolveEnd cannot handle substitution $s")
  def notyet(c: Constraint) = freshConstraintSystem addNewConstraint (c)
  def never(c: Constraint) = SolveEndCanonicalBoundsCS(defaultBounds, Seq(c))
}

case class SolveEndCanonicalBoundsCS(bounds: Map[Symbol, (LBound, UBound)], never: Seq[Constraint]) extends ConstraintSystem[SolveEndCanonicalBoundsCS] {
  //invariant: substitution maps to ground types
  //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct

  implicit val csf = SolveEndCanonicalBounds
  import csf.state

  val substitution: TSubst = Map()

  def notyet = {
    var cons = Seq[Constraint]()
    for ((x, (l, u)) <- bounds) {
      val join = subtype.Join(UVar(x), l.nonground ++ l.ground.toSet)
      val meet = subtype.Meet(UVar(x), u.nonground ++ u.ground.toSet)
      cons = cons :+ join :+ meet
    }
    cons
  }

  def mergeSubsystem(that: SolveEndCanonicalBoundsCS) = {
    var current = SolveEndCanonicalBoundsCS(bounds, never ++ that.never)

    for((tv, (thatL, thatU)) <- that.bounds) {
      if (bounds.isDefinedAt(tv)) {
        for (t <- thatL.nonground ++ thatL.ground.toSet)
          current = current.addLowerBound(tv, t)
        for (t <- thatU.nonground ++ thatU.ground.toSet)
          current = current.addUpperBound(tv, t)
      }
      else
        current = SolveEndCanonicalBoundsCS(current.bounds + (tv -> (thatL, thatU)), current.never)
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
      cons.foldLeft(this)((cs, c) => cs mergeSubsystem c.solve(cs))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def tryFinalize =
    SolveContinuously.state.withValue(state.value) {
      SolveContinuouslyCS(Map(), bounds, never).tryFinalize
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
    val cs = SolveEndCanonicalBoundsCS(newbounds, newnever)

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
    val cs = SolveEndCanonicalBoundsCS(newbounds, newnever)

    cs mergeSubsystem (subtype.Join(changed, lower.nonground ++ lower.ground.toSet).solve(cs))
  }


  def applyPartialSolution(t: Type) = t

  def applyPartialSolutionIt[U, C <: Iterable[U]](it: C, f: U=>Type)(implicit bf: CanBuildFrom[Iterable[U], (U, Type), C])
  = it

  def propagate = this
}