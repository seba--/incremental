package incremental

import incremental.ConstraintOps.CSet
import incremental.Type.Companion.UVar
import incremental.Type.Companion._
import incremental.pcf.UVar

import scala.util.DynamicVariable

/**
 * Created by seba on 13/11/14.
 */
object ConstraintOps extends ConstraintDefs[Type] {
  type Constraint = incremental.Constraint
  type NotYetSolvable = Seq[Constraint]
  type Unsolvable = Seq[Constraint]

  case class CSet(substitution: TSubst, notyet: NotYetSolvable, never: Unsolvable) extends CSetAlg[CSet] {
    def unsolved = notyet ++ never

    def isSolved = notyet.isEmpty && never.isEmpty

    def solvable = !never.isEmpty

    def ++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        var msolution = substitution mapValues (_.subst(other.substitution))
        val mnotyet = notyet ++ other.notyet
        var mnever = never ++ other.never

        for ((x, t2) <- other.substitution) {
          msolution.get(x) match {
            case None => msolution += x -> t2.subst(msolution)
            case Some(t1) =>
              val usol = t1.unify(t2, msolution)
              msolution = msolution.mapValues(_.subst(usol.substitution)) ++ usol.substitution
              mnever = mnever ++ usol.never
          }
        }

        CSet(msolution, mnotyet, mnever)
      }
      state.value.stats.mergeSolutionTime += time
      res
    }

    // if solutions are not needed
    def +++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        CSet(Map(), mnotyet, mnever)
      }
      state.value.stats.mergeSolutionTime += time
      res
    }

    def ++++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        val msolution = substitution ++ other.substitution
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        CSet(msolution, mnotyet, mnever)
      }
      state.value.stats.mergeSolutionTime += time
      res
    }

    def <++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        var mnotyet = notyet.map(_.subst(other.substitution)) ++ other.notyet
        var mnever = never.map(_.subst(other.substitution)) ++ other.never
        CSet(Map(), mnotyet, mnever)
      }
      state.value.stats.mergeSolutionTime += time
      res
    }

    def +(that: Constraint): CSet = {
      state.value.stats.constraintCount += 1
      val (res, time) = Util.timed(this ++ that.solve(this))
      state.value.stats.constraintSolveTime += time
      res
    }

    def ++(cs: Iterable[Constraint]): CSet = {
      state.value.stats.constraintCount += cs.size
      val (res, time) = Util.timed {
        cs.foldLeft(this)((sol,c) => sol ++ c.solve(sol))
      }
      state.value.stats.constraintSolveTime += time
      res
    }

    def isSolvable: Boolean = never.isEmpty
    def solution = (substitution, notyet, never)
    def trySolve: incremental.ConstraintOps.CSet = trySolveNow

    private def trySolve(finalize: Boolean) = {
      var rest = notyet
      var newSolution = substitution
      var newNotyet = Seq[Constraint]()
      var newNever = never
      while (!rest.isEmpty) {
        val next = rest.head
        rest = rest.tail
        val wasNotyet = newNotyet ++ rest
        val current = CSet(newSolution, wasNotyet, newNever)
        val sol = if (finalize) next.finalize(current) else next.solve(current)

        newSolution = newSolution.mapValues(_.subst(sol.substitution)) ++ sol.substitution
        newNever = newNever ++ sol.never
        newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
      }
      CSet(newSolution, newNotyet, newNever)
    }

    def trySolveNow = trySolve(false)
    def tryFinalize = trySolve(true)
  }

  class Gen extends GenBase {
    type V = UVar
    private var _nextId = 0
    def freshUVar(): UVar = {
      val v = UVar(Symbol("x$" + _nextId))
      _nextId += 1
      v
    }
  }

  def freshState = new State(new Gen, new Statistics)

  def emptyCSet = CSet(Map(), Seq(), Seq())
  def solution(s: TSubst): CSet = CSet(s, Seq(), Seq())
  def notyet(c: Constraint): CSet = CSet(Map(), Seq(c), Seq())
  def never(c: Constraint): CSet = CSet(Map(), Seq(), Seq(c))

  def _mergeReqMaps(reqs1: Requirements, reqs2: Requirements) = {
    var mcons = Seq[EqConstraint]()
    var mreqs = reqs1
    for ((x, r2) <- reqs2)
      reqs1.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = EqConstraint(r1, r2) +: mcons
      }

    (mcons, mreqs)
  }
}

trait Constraint {
  def solve(s: CSet): CSet
  def subst(s: TSubst): Constraint
  def finalize(s: CSet): CSet
}
case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve(s: CSet) = expected.unify(actual, s.substitution)
  def finalize(s: CSet) = solve(s)
  def subst(s: TSubst) = EqConstraint(expected.subst(s), actual.subst(s))
}

class Statistics {
  var preparationTime = 0.0
  var typecheckTime = 0.0
  var mergeSolutionTime = 0.0
  var constraintCount = 0
  var constraintSolveTime = 0.0
  var mergeReqsTime = 0.0
}

abstract class ConstraintDefs[Type <: Typ[Type]](implicit val definitions: TypCompanion[Type]) {
  final type TSubst = definitions.TSubst

  type Constraint
  type Requirements = Map[Symbol, Type]
  type Solution = (TSubst, NotYetSolvable, Unsolvable)
  type NotYetSolvable
  type Unsolvable
  type CSet <: CSetAlg[CSet]

  trait CSetAlg[CS] {
    def isSolved: Boolean
    def solution: Solution
    def substitution: TSubst
    def notyet: NotYetSolvable
    def never: Unsolvable
    def ++(that: CS): CS
    def + (that: Constraint): CS
    def ++(cs: Iterable[Constraint]): CS
    def tryFinalize: CS
  }

  trait GenBase {
    type V <: Type
    def freshUVar(): V
  }
  type Gen <: GenBase

  class State(val gen: Gen, val stats: Statistics)
  def freshState: State
  val state: DynamicVariable[State] = new DynamicVariable[State](new State(???, ???))

  def emptyCSet: CSet
  def solution(s: TSubst): CSet
  def notyet(c: Constraint): CSet
  def never(c: Constraint): CSet

  final def mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[Constraint], Requirements) = {
    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
    state.value.stats.mergeReqsTime += time
    res
  }

  def _mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[Constraint], Requirements)
}
