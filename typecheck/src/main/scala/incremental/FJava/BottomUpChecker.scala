package incremental.FJava

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type.Name
import incremental._
import incremental.systemf.{Var, TNum, Num, ConstraintOps}


/**
 * Created by lirakuci on 3/2/15.
 */
class BottomUpChecker extends TypeChecker[Type] {
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

  type Result = (Type, Reqs, CReqs, Solution)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    //    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    //    preparationTime += ptime

    val (res, ctime) = Util.timed {
      root.visitUninitialized { e =>
        e.typ = typecheckStep(e)
        true
      }

      val (t_, reqs, creqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!creqs.isEmpty)
        Right(s"Unresolved type variables requirements $creqs, type variables $t, tunres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
    typecheckTime += ctime
    res

    def typecheckStep(e: Exp_[Result]): Result = e.kind match {

      case Var =>
        val x = e.lits(0).asInstanceOf[Symbol]
        val X = freshUVar()
        (X, Map(x -> X), Map(), emptySol)

    }
  }
}
