package incremental.haskell

import constraints.{CVar, ConstraintSystemFactory, GenBase}
import constraints.{ConstraintSystem, _}
import incremental.MyBuilder
import incremental.haskell.Node.Node
import constraints.equality.Type

import scala.collection.generic.CanBuildFrom

/**
 * Created by seba on 13/11/14.
 */
abstract class  TypeChecker[G <: GenBase, C, CS <: ConstraintSystem[G, C, CS]] extends Serializable {
  type TError
  type Signature
  type T = Type
  type CSFactory <: ConstraintSystemFactory[G, C, CS]
  implicit val csFactory: CSFactory

  def typecheck(e: Node): Either[T, TError] = {
    localState.resetStats()
    csFactory.state.value = localState
    typecheckImpl(e)
  }

  protected def typecheckImpl(e: Node): Either[T, TError]


  implicit def bf[K,V] = new CanBuildFrom[Iterable[(K,V)], ((K, V), V), Map[K, V]] {
    def apply = new MyBuilder
    def apply(from: Iterable[(K,V)]) = from.foldLeft(new MyBuilder[K,V])((b, p) => b += ((p) -> p._2))
  }


  lazy val localState = csFactory.freshState

  def gen: G = localState.gen

  def freshSymbol[T](prefix: String): CVar[T] = localState.gen.freshSymbol(prefix)

  def freshUVar() = UVar(freshSymbol("x$"))

  def freshSchemaVar() = TSchemaVar(freshSymbol("x$"))

  def freshTVar() = TVar(freshSymbol("x$").x)

}

trait TypeCheckerFactory[G <: GenBase, C, CS <: ConstraintSystem[G, C, CS]] {
  def makeChecker: TypeChecker[G, C, CS]
}
