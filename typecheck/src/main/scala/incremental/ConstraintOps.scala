package incremental

import incremental.ConstraintOps.Solution
import incremental.Type._

/**
 * Created by seba on 13/11/14.
 */
object ConstraintOps {
  var mergeSolutionTime = 0.0
  var constraintCount = 0
  var constraintSolveTime = 0.0

  type NotYetSolvable = Seq[Constraint]
  type Unsolvable = Seq[Constraint]

  val emptySol: Solution = Solution(Map(), Seq(), Seq())
  def solution(s: TSubst) = Solution(s, Seq(), Seq())
  def notyet(c: Constraint) = Solution(Map(), Seq(c), Seq())
  def never(c: Constraint) = Solution(Map(), Seq(), Seq(c))
  case class Solution(solution: TSubst, notyet: NotYetSolvable, never: Unsolvable) {
    def unsolved = notyet ++ never
    def isSolved = notyet.isEmpty && never.isEmpty
    def solvable = !never.isEmpty

    def ++(other: Solution): Solution = {
      val (res, time) = Util.timed {
        var msolution = solution mapValues (_.subst(other.solution))
        var mnotyet = never ++ other.never
        var mnever = never ++ other.never

        for ((x, t2) <- other.solution) {
          msolution.get(x) match {
            case None => msolution += x -> t2.subst(msolution)
            case Some(t1) =>
              val usol = t1.unify(t2, msolution)
              msolution = msolution.mapValues(_.subst(usol.solution)) ++ usol.solution
              mnotyet = mnotyet ++ usol.notyet
              mnever = mnever ++ usol.never
          }
        }

        Solution(msolution, mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    // if solution.keys != other.solution.keys
    def +++(other: Solution): Solution = {
      val (res, time) = Util.timed {
        var msolution = solution ++ other.solution
        var mnotyet = never ++ other.never
        var mnever = never ++ other.never
        Solution(msolution, mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    def trySolveNow: Solution = {
      var rest = notyet
      var newSolution = solution
      var newNotyet = Seq[Constraint]()
      var newNever = never
      while (!rest.isEmpty) {
        val next = rest.head
        rest = rest.tail
        val wasNotyet = newNotyet ++ rest
        val current = Solution(newSolution, wasNotyet, newNever)
        val sol = next.solve(current)

        newSolution = sol.solution
        newNever = sol.never
        newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
      }
      Solution(newSolution, newNotyet, newNever)
    }
  }

  def solve(cs: Iterable[Constraint], sol: Solution = emptySol): Solution = {
    constraintCount += cs.size
    val (res, time) = Util.timed(cs.foldLeft(sol)((sol,c) => sol ++ c.solve(sol)))
    constraintSolveTime += time
    res
  }
  def solve(c: Constraint): Solution = {
    constraintCount += 1
    val (res, time) = Util.timed(c.solve(emptySol))
    constraintSolveTime += time
    res
  }
  def solve(c: Constraint, sol: Solution): Solution = {
    constraintCount += 1
    val (res, time) = Util.timed(sol ++ c.solve(sol))
    constraintSolveTime += time
    res
  }

}


trait Constraint {
  def solve(s: Solution): Solution
  def equiv(other: Constraint, s: TSubst): Boolean
}
case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve(s: Solution) = expected.unify(actual, s.solution)
  def equiv(other: Constraint, s: TSubst) = other match {
    case EqConstraint(e, a) => expected.unify(e, s) == Some(Map()) && actual.unify(a, s) == Some(Map())
    case _ => false
  }
}
