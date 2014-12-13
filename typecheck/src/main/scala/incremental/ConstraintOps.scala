package incremental

import incremental.ConstraintOps.CSet
import incremental.Type.Companion._

/**
 * Created by seba on 13/11/14.
 */
object ConstraintOps extends ConstraintDefs[Type] {
  type Constraint = incremental.Constraint
  type NotYetSolvable = Seq[Constraint]
  type Unsolvable = Seq[Constraint]

  def emptySol = CSet(Map(), Seq(), Seq())
  def solution(s: TSubst) = CSet(s, Seq(), Seq())
  def notyet(c: Constraint) = CSet(Map(), Seq(c), Seq())
  def never(c: Constraint) = CSet(Map(), Seq(), Seq(c))

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
      mergeSolutionTime += time
      res
    }

    // if solutions are not needed
    def +++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        CSet(Map(), mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    def ++++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        val msolution = substitution ++ other.substitution
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        CSet(msolution, mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    def <++(other: CSet): CSet = {
      val (res, time) = Util.timed {
        var mnotyet = notyet.map(_.subst(other.substitution)) ++ other.notyet
        var mnever = never.map(_.subst(other.substitution)) ++ other.never
        CSet(Map(), mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    def +(that: Constraint): CSet = CSet(substitution, that +: notyet, never)
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

  def solve(cs: Iterable[Constraint], sol: CSet = emptySol): CSet = {
    constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(sol)((sol,c) => sol ++ c.solve(sol))
    }
    constraintSolveTime += time
    res
  }
  def solve(c: Constraint): CSet = {
    constraintCount += 1
    val (res, time) = Util.timed(c.solve(emptySol))
    constraintSolveTime += time
    res
  }
  def solve(c: Constraint, sol: CSet): CSet = {
    constraintCount += 1
    val (res, time) = Util.timed(sol ++ c.solve(sol))
    constraintSolveTime += time
    res
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

  var mergeSolutionTime = 0.0
  var constraintCount = 0
  var constraintSolveTime = 0.0

  type Constraint
  type Requirements = Map[Symbol, Type]
  type Solution = (TSubst, NotYetSolvable, Unsolvable)
  type NotYetSolvable
  type Unsolvable
  type CSet <: CSetAlg[CSet]

  trait CSetAlg[CS] {
    def isSolved: Boolean
    def isSolvable: Boolean
    def solution: Solution
    def substitution: TSubst
    def notyet: NotYetSolvable
    def never: Unsolvable
    def ++(that: CS): CS
    def +++(that: CS): CS
    def ++++(that: CS): CS
    def <++(that: CS): CS
    def + (that: Constraint): CS
    def tryFinalize: CS
    def trySolve: CS
  }

  trait GenBase {
    type V <: Type
    def freshUVar(): V
  }
  type Gen <: GenBase
}

trait ConstraintSystem[Type <: Typ[Type], CDef <: ConstraintDefs[Type]] {
  val defs: CDef
  import defs._

  implicit val stats: Statistics
  implicit val gen: defs.Gen

  def mergeReqMaps(reqs1: Requirements, reqs2: Requirements) = {
    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
    stats.mergeReqsTime += time
    res
  }

  def _mergeReqMaps(reqs1: Requirements, reqs2: Requirements): (defs.CSet, Requirements)

  def emptyCSet: defs.CSet
}