package incremental

import constraints._
import incremental.Node.Node

/**
 * Created by seba on 13/11/14.
 */
abstract class TypeChecker[G <: GenBase, C, CS <: ConstraintSystem[G, C, CS]] extends Serializable {
  type T
  type TError
  type Result

  type CSFactory <: ConstraintSystemFactory[G, C, CS]
  implicit val csFactory: CSFactory

  lazy val localState = csFactory.freshState
  def gen: G = localState.gen
  def freshSymbol[T](prefix: String): CVar[T] = localState.gen.freshSymbol(prefix)

  def typecheck(e: Node[C, Result]): Either[T, TError] = {
    localState.resetStats()
    csFactory.state.value = localState
    typecheckImpl(e)
  }

  protected def typecheckImpl(e: Node[C, Result]): Either[T, TError]
}

trait TypeCheckerFactory[G <: GenBase, C, CS <: ConstraintSystem[G, C, CS]] {
  def makeChecker: TypeChecker[G, C, CS]
}