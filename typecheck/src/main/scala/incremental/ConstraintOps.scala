package incremental

import incremental.ConstraintOps.Solution
import incremental.Type.Companion._

/**
 * Created by seba on 13/11/14.
 */
object ConstraintOps {
  var mergeSolutionTime = 0.0
  var constraintCount = 0
  var constraintSolveTime = 0.0

  type NotYetSolvable = Seq[Constraint]
  type Unsolvable = Seq[Constraint]

  def emptySol = Solution(Map(), Seq(), Seq())
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
        val mnotyet = notyet ++ other.notyet
        var mnever = never ++ other.never

        for ((x, t2) <- other.solution) {
          msolution.get(x) match {
            case None => msolution += x -> t2.subst(msolution)
            case Some(t1) =>
              val usol = t1.unify(t2, msolution)
              msolution = msolution.mapValues(_.subst(usol.solution)) ++ usol.solution
              mnever = mnever ++ usol.never
          }
        }

        Solution(msolution, mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    // if solutions are not needed
    def +++(other: Solution): Solution = {
      val (res, time) = Util.timed {
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        Solution(Map(), mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    def ++++(other: Solution): Solution = {
      val (res, time) = Util.timed {
        val msolution = solution ++ other.solution
        val mnotyet = notyet ++ other.notyet
        val mnever = never ++ other.never
        Solution(msolution, mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    def <++(other: Solution): Solution = {
      val (res, time) = Util.timed {
        var mnotyet = notyet.map(_.subst(other.solution)) ++ other.notyet
        var mnever = never.map(_.subst(other.solution)) ++ other.never
        Solution(Map(), mnotyet, mnever)
      }
      mergeSolutionTime += time
      res
    }

    private def trySolve(finalize: Boolean) = {
      var rest = notyet
      var newSolution = solution
      var newNotyet = Seq[Constraint]()
      var newNever = never
      while (!rest.isEmpty) {
        val next = rest.head
        rest = rest.tail
        val wasNotyet = newNotyet ++ rest
        val current = Solution(newSolution, wasNotyet, newNever)
        val sol = if (finalize) next.finalize(current) else next.solve(current)

        newSolution = newSolution.mapValues(_.subst(sol.solution)) ++ sol.solution
        newNever = newNever ++ sol.never
        newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
      }
      Solution(newSolution, newNotyet, newNever)
    }

    def trySolveNow = trySolve(false)
    def tryFinalize = trySolve(true)
  }

  def solve(cs: Iterable[Constraint], sol: Solution = emptySol): Solution = {
    constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(sol)((sol,c) => sol ++ c.solve(sol))
    }
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
  def subst(s: TSubst): Constraint
  def finalize(s: Solution): Solution
}
case class EqConstraint(expected: Type, actual: Type) extends Constraint {
  def solve(s: Solution) = expected.unify(actual, s.solution)
  def finalize(s: Solution) = solve(s)
  def subst(s: TSubst) = EqConstraint(expected.subst(s), actual.subst(s))
}
