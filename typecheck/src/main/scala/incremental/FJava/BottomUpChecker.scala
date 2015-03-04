package incremental.FJava

import incremental.ConstraintOps.Solution
import incremental.Exp._
import incremental.{Name, Util, TypeChecker, Type}
import incremental.systemf.ConstraintOps

/**
 * Created by lirakuci on 3/2/15.
 */
class BottomUpChecker extends TypeChecker[Type]{
  val constraint = new ConstraintOps
  import constraint._

  var preparationTime = 0.0
  var typecheckTime = 0.0
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def tmergeReqsTime = constraint.tmergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime
  def mergeSolutionTime = constraint.mergeSolutionTime

  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Name, Class]

  type TReqs = Set[Symbol]

  type TCReqs = List[Name]

  type Result = (Type, Reqs, TReqs, Solution)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    //    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    //    preparationTime += ptime

    val (res, ctime) = Util.timed {
      root.visitUninitialized { e =>
        e.typ = typecheckStep(e)
        true
      }

      val (t_, reqs, treqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!treqs.isEmpty)
        Right(s"Unresolved type variables requirements $treqs, type variables $t, tunres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
    typecheckTime += ctime
    res
  }
}
