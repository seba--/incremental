package constraints.equality

import constraints.equality.Type._
import ConstraintSystemFactory._
import incremental.Util

case class ConstraintSystem(substitution: TSubst, notyet: NotYetSolvable, never: Unsolvable) extends constraints.ConstraintSystem[ConstraintSystem, EqConstraint] {
  def unsolved = notyet ++ never

  def isSolved = notyet.isEmpty && never.isEmpty

  def solvable = !never.isEmpty

  def ++(other: ConstraintSystem): ConstraintSystem = {
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

      ConstraintSystem(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  // if solutions are not needed
  def +++(other: ConstraintSystem): ConstraintSystem = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      ConstraintSystem(Map(), mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def ++++(other: ConstraintSystem): ConstraintSystem = {
    val (res, time) = Util.timed {
      val msolution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      ConstraintSystem(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def <++(other: ConstraintSystem): ConstraintSystem = {
    val (res, time) = Util.timed {
      var mnotyet = notyet.map(_.subst(other.substitution)) ++ other.notyet
      var mnever = never.map(_.subst(other.substitution)) ++ other.never
      ConstraintSystem(Map(), mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def +(that: Constraint): ConstraintSystem = {
    state.value.stats.constraintCount += 1
    val (res, time) = Util.timed(this ++ that.solve(this))
    state.value.stats.constraintSolveTime += time
    res
  }

  def ++(cs: Iterable[Constraint]): ConstraintSystem = {
    state.value.stats.constraintCount += cs.size
    val (res, time) = Util.timed {
      cs.foldLeft(this)((sol,c) => sol ++ c.solve(sol))
    }
    state.value.stats.constraintSolveTime += time
    res
  }

  def isSolvable: Boolean = never.isEmpty
  def solution = (substitution, notyet, never)
  def trySolve: ConstraintSystem = trySolveNow

  private def trySolve(finalize: Boolean) = {
    var rest = notyet
    var newSolution = substitution
    var newNotyet = Seq[Constraint]()
    var newNever = never
    while (!rest.isEmpty) {
      val next = rest.head
      rest = rest.tail
      val wasNotyet = newNotyet ++ rest
      val current = ConstraintSystem(newSolution, wasNotyet, newNever)
      val sol = if (finalize) next.finalize(current) else next.solve(current)

      newSolution = newSolution.mapValues(_.subst(sol.substitution)) ++ sol.substitution
      newNever = newNever ++ sol.never
      newNotyet = newNotyet ++ (sol.notyet diff wasNotyet)
    }
    ConstraintSystem(newSolution, newNotyet, newNever)
  }

  def trySolveNow = trySolve(false)
  def tryFinalize = trySolve(true)
}
