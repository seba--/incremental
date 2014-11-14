package incremental

import incremental.Type.TError
import incremental.Exp.Exp

/**
 * Created by seba on 13/11/14.
 */
trait TypeChecker {
  def typecheck(e: Exp): Either[Type, TError]

  def preparationTime: Double
  def typecheckTime: Double
  def constraintCount: Int
  def mergeReqsTime: Double
  def constraintSolveTime: Double
}

trait TypeCheckerFactory {
  def makeChecker: TypeChecker
}