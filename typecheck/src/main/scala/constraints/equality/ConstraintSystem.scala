package constraints.equality

import Type.Companion._
import incremental.Util

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystem[CS, EqConstraint[CS], Type[CS]] {
  val csFactory: ConstraintSystemFactory[CS]
  import csFactory.state
  import csFactory.system

  def substitution: TSubst[CS]
  def notyet: Seq[EqConstraint[CS]]
  def never: Seq[EqConstraint[CS]]

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty

//  def mergeSubsystem(other: CS): CS = mergeSubsystem(other.asInstanceOf[ConstraintSystem[CS]])

//  def mergeSubsystem(other: ConstraintSystem[CS]): CS = {
//    val (res, time) = Util.timed {
//      var msolution = substitution mapValues (_.subst(other.substitution))
//      val mnotyet = notyet ++ other.notyet
//      var mnever = never ++ other.never
//
//      for ((x, t2) <- other.substitution) {
//        msolution.get(x) match {
//          case None => msolution += x -> t2.subst(msolution)
//          case Some(t1) =>
//            val usol = t1.unify(t2, msolution)
//            msolution = msolution.mapValues(_.subst(usol.substitution)) ++ usol.substitution
//            mnever = mnever ++ usol.never
//        }
//      }
//
//      system(msolution, mnotyet, mnever)
//    }
//    state.value.stats.mergeSolutionTime += time
//    res
//  }

  def mergeApply(other: ConstraintSystem[CS]): CS = {
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

      system(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  // if solutions are not needed
  def +++(other: ConstraintSystem[CS]): CS = {
    val (res, time) = Util.timed {
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      system(Map(), mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def ++++(other: ConstraintSystem[CS]): CS = {
    val (res, time) = Util.timed {
      val msolution = substitution ++ other.substitution
      val mnotyet = notyet ++ other.notyet
      val mnever = never ++ other.never
      system(msolution, mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

  def <++(other: ConstraintSystem[CS]): CS = {
    val (res, time) = Util.timed {
      var mnotyet = notyet.map(_.subst(other.substitution)) ++ other.notyet
      var mnever = never.map(_.subst(other.substitution)) ++ other.never
      system(Map(), mnotyet, mnever)
    }
    state.value.stats.mergeSolutionTime += time
    res
  }

//  def addNewConstraint(that: EqConstraint[CS]): CS = {
//    state.value.stats.constraintCount += 1
//    val (res, time) = Util.timed(this mergeApply that.solve(this))
//    state.value.stats.constraintSolveTime += time
//    res
//  }

//  def addNewConstraints(cs: Iterable[EqConstraint[CS]]): CS = {
//    state.value.stats.constraintCount += cs.size
//    val (res, time) = Util.timed {
//      if (cs.isEmpty)
//        system(substitution, notyet, never)
//      else {
//        val init = this mergeApply cs.head.solve(this)
//        cs.tail.foldLeft(init)((sol, c) => sol mergeApply c.solve(sol))
//      }
//    }
//    state.value.stats.constraintSolveTime += time
//    res
//  }

//  def applyPartialSolution(t: Type[CS]) = t.subst(substitution)

//  def propagate = system(substitution, notyet, never)

  def isSolvable: Boolean = never.isEmpty
  def solution = (substitution, notyet, never)

//  protected def trySolve(finalize: Boolean): CS
//  def trySolveNow = trySolve(false)
//  def trySolve: CS = trySolveNow
}
