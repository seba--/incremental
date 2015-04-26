package incremental

import constraints._
import incremental.Node.Node

/**
 * Created by seba on 13/11/14.
 */
abstract class TypeChecker[T <: Type, V <: T] extends Serializable {
  type TError
  type Constraint
  type CS <: ConstraintSystem[CS, Constraint,T]
  type CSFactory <: ConstraintSystemFactory[T, V, Constraint, CS]
  val csFactory: CSFactory

  lazy val localState: State[V] = csFactory.freshState

  def preparationTime: Double = localState.stats.preparationTime
  def typecheckTime: Double = localState.stats.typecheckTime
  def constraintCount: Double = localState.stats.constraintCount
  def mergeReqsTime: Double = localState.stats.mergeReqsTime
  def constraintSolveTime: Double = localState.stats.constraintSolveTime
  def mergeSolutionTime: Double = localState.stats.mergeSolutionTime

  def freshUVar() = localState.gen.freshUVar()


  def typecheck(e: Node): Either[T, TError] = {
    csFactory.state.value = localState
    typecheckImpl(e)
  }

  protected def typecheckImpl(e: Node): Either[T, TError]
}

trait TypeCheckerFactory[T <: Type, V <: T] {
  def makeChecker: TypeChecker[T, V]
}