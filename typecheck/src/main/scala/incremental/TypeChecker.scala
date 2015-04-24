package incremental

import constraints._
import incremental.Node.Node

/**
 * Created by seba on 13/11/14.
 */
abstract class TypeChecker[Type <: Typ[Type]](implicit val definitions: TypCompanion[Type]) extends Serializable {
  final type TError = definitions.TError
  final type TSubst = definitions.TSubst

  def typecheck(e: Node): Either[Type, TError]

  def preparationTime: Double
  def typecheckTime: Double
  def constraintCount: Int
  def mergeReqsTime: Double
  def constraintSolveTime: Double
  def mergeSolutionTime: Double
}

trait TypeCheckerFactory[T <: Typ[T]] {
  def makeChecker: TypeChecker[T]
}