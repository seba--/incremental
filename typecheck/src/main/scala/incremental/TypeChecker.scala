package incremental

import incremental.Exp.Exp

/**
 * Created by seba on 13/11/14.
 */
abstract class TypeChecker[Type <: Typ[Type]](implicit val definitions: TypCompanion[Type]) extends Serializable {
  final type TError = definitions.TError
  final type TSubst = definitions.TSubst
  type Result
  type CSystem <: ConstraintSystem[Type]
  val cs: CSystem

  lazy val localState = cs.freshState

  def preparationTime: Double = localState.stats.preparationTime
  def typecheckTime: Double = localState.stats.typecheckTime
  def constraintCount: Double = localState.stats.constraintCount
  def mergeReqsTime: Double = localState.stats.mergeReqsTime
  def constraintSolveTime: Double = localState.stats.constraintSolveTime
  def mergeSolutionTime: Double = localState.stats.mergeSolutionTime

  def typecheck(e: Exp): Either[Type, TError]
}

trait TypeCheckerFactory[T <: Typ[T]] {
  def makeChecker: TypeChecker[T]
}

abstract class BUChecker[Type <: Typ[Type]](implicit definitions: TypCompanion[Type]) extends TypeChecker[Type]()(definitions) {
  type Result = (Type, cs.Requirements, cs.CSet)
  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    //    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    //    preparationTime += ptime
    cs.state.withValue(localState) {
      val (res, ctime) = Util.timed {
        root.visitUninitialized {e =>
          e.typ = typecheckStep(e)
          true
        }

        val (t_, reqs, sol_) = root.typ
        val sol = sol_.tryFinalize
        val t = t_.subst(sol.substitution)

        if (!reqs.isEmpty)
          Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.notyet}, unsat ${sol.never}")
        else if (!sol.isSolved)
          Right(s"Unresolved constraints ${sol.notyet}, unsat ${sol.never}, type $t")
        else
          Left(t)
      }
      localState.stats.typecheckTime += ctime
      res
    }
  }

  def typecheckStep(e: Exp_[Result]): Result
}

abstract class DUChecker[Type <: Typ[Type]](implicit definitions: TypCompanion[Type]) extends TypeChecker[Type]()(definitions) {
  type Result = (Type, cs.CSet)

  def typecheck(e: Exp): Either[Type, TError] = {
    cs.state.withValue(localState) {
      val root = e.withType[Result]
      val (res, ctime) = Util.timed {
        try {
          val (t, sol_) = typecheck(root, Map())
          val sol = sol_.tryFinalize
          if (sol.isSolved)
            Left(t.subst(sol.substitution))
          else
            Right(s"Unresolved constraints ${sol.notyet}, unsat ${sol.never}, type ${t.subst(sol.substitution)}, subst ${sol.substitution}")
        } catch {
          case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
        }
      }
      localState.stats.typecheckTime += ctime
      res
    }
  }

  def typecheck(e: Exp_[Result], ctx: TSubst): Result

  case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException
}
