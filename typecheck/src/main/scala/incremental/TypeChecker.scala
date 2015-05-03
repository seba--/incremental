package incremental

import constraints._
import incremental.Node.Node

/**
 * Created by seba on 13/11/14.
 */
abstract class TypeChecker[T <: Type, V <: T, G <: GenBase[V], Constraint, CS <: ConstraintSystem[CS, Constraint, T, V, G]] extends Serializable {
  type TError
//  type Constraint
//  type CS <: ConstraintSystem[CS, Constraint,T]
  type CSFactory <: ConstraintSystemFactory[T, V, G, Constraint, CS]
  implicit val csFactory: CSFactory

  lazy val localState = csFactory.freshState
  def gen: G = localState.gen

  def preparationTime: Double = localState.stats.preparationTime
  def typecheckTime: Double = localState.stats.typecheckTime
  def constraintCount: Double = localState.stats.constraintCount
  def mergeReqsTime: Double = localState.stats.mergeReqsTime
  def constraintSolveTime: Double = localState.stats.constraintSolveTime
  def mergeSolutionTime: Double = localState.stats.mergeSolutionTime
  def finalizeTime: Double = localState.stats.finalizeTime

  def freshUVar(): V = localState.gen.freshUVar()


  def typecheck(e: Node): Either[T, TError] = {
    localState.stats.reset
    csFactory.state.value = localState
    typecheckImpl(e)
  }

  protected def typecheckImpl(e: Node): Either[T, TError]
}

trait TypeCheckerFactory[T <: Type, V <: T, G <: GenBase[V], Constraint, CS <: ConstraintSystem[CS, Constraint, T, V, G]] {
  def makeChecker: TypeChecker[T, V, G, Constraint, CS]
}