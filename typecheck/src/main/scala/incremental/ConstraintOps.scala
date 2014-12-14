package incremental

import incremental.ConstraintOps.CSet
import incremental.Type.Companion.UVar
import incremental.Type.Companion._
import incremental.pcf.UVar

/**
 * Created by seba on 13/11/14.
 */
object ConstraintOps extends ConstraintDefs[Type] {
  type Constraint = incremental.Constraint
  type NotYetSolvable = Seq[Constraint]
  type Unsolvable = Seq[Constraint]

  class Gen {
    private var _nextId = 0
    def freshUVar(): UVar = {
      val v = UVar(Symbol("x$" + _nextId))
      _nextId += 1
      v
    }
  }

  case class CSet(substitution: TSubst, notyet: NotYetSolvable, never: Unsolvable)(implicit stat: Statistics) extends CSetAlg[CSet] {
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
      stat.mergeSolutionTime += time
      res
    }

    // if solutions are not needed
    def +++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        CSet(Map(), mnotyet, mnever)
      }
      stat.mergeSolutionTime += time
      res
    }

    def ++++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        val msolution = substitution ++ other.substitution
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        CSet(msolution, mnotyet, mnever)
      }
      stat.mergeSolutionTime += time
      res
    }

    def <++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        var mnotyet = notyet.map(_.subst(other.substitution)) ++ other.notyet
        var mnever = never.map(_.subst(other.substitution)) ++ other.never
        CSet(Map(), mnotyet, mnever)
      }
      stat.mergeSolutionTime += time
      res
    }

    def +(that: Constraint): CSet = CSet(substitution, that +: notyet, never)

    def +!(that: Constraint): CSet = {
      stat.constraintCount += 1
      val (res, time) = Util.timed(this ++ that.solve(this))
      stat.constraintSolveTime += time
      res
    }

    def ++!(cs: Iterable[Constraint]): CSet = {
      stat.constraintCount += cs.size
      val (res, time) = Util.timed {
        cs.foldLeft(this)((sol,c) => sol ++ c.solve(sol))
      }
      stat.constraintSolveTime += time
      res
    }

    def ++++(cs: Iterable[Constraint]): CSet = {
      stat.constraintCount += cs.size
      val (res, time) = Util.timed {
        cs.foldLeft(this)((sol,c) => sol ++ c.solve(sol))
      }
      stat.constraintSolveTime += time
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

//TODO make TypeChecker depend on Statistics
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
  final type TError = definitions.TError

 /* var mergeSolutionTime = 0.0
  var constraintCount = 0
  var constraintSolveTime = 0.0*/

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
    def +++(that: CS): CS
    def ++++(that: CS): CS
    def <++(that: CS): CS
    def + (that: Constraint): CS
    def ++++(cs: Iterable[Constraint]): CS
    def tryFinalize: CS
    def trySolve: CS
  }

  trait GenBase {
    def freshUVar(): definitions.UVar
  }
  type Gen <: GenBase
}

trait ConstraintSystem[Type <: Typ[Type], CDef <: ConstraintDefs[Type]] {
  val defs: CDef
  import defs._

  implicit val stats: Statistics
  implicit val gen: defs.Gen

  final def mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[defs.Constraint], Requirements) = {
    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
    stats.mergeReqsTime += time
    res
  }

  def _mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (Seq[defs.Constraint], Requirements)

  def emptyCSet: defs.CSet
}

class DefaultConstraintSystem extends ConstraintSystem[Type, ConstraintOps.type] {
  val defs = ConstraintOps
  import defs._

  def emptySol = emptyCSet
  def solution(s: TSubst) = CSet(s, Seq(), Seq())
  def notyet(c: defs.Constraint) = CSet(Map(), Seq(c), Seq())
  def never(c: defs.Constraint) = CSet(Map(), Seq(), Seq(c))

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

  def emptyCSet = CSet(Map(), Seq(), Seq())

  implicit val gen = new Gen
  implicit val stats = new Statistics
}
