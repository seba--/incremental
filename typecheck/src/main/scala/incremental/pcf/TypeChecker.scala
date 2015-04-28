package incremental.pcf

import constraints.equality._
import incremental.MyBuilder
import incremental.Node.Node

import scala.collection.generic.CanBuildFrom

/**
 * Created by seba on 13/11/14.
 */
abstract class TypeChecker[CS <: ConstraintSystem[CS]] extends incremental.TypeChecker[Type, UVar, EqConstraint, CS] {
  type CSFactory <: ConstraintSystemFactory[CS]
  implicit val csFactory: CSFactory

  implicit def bf[K,V] = new CanBuildFrom[Iterable[(K,V)], ((K, V), V), Map[K, V]] {
    def apply = new MyBuilder
    def apply(from: Iterable[(K,V)]) = from.foldLeft(new MyBuilder[K,V])((b, p) => b += ((p) -> p._2))
  }
}

trait TypeCheckerFactory[CS <: ConstraintSystem[CS]] {
  def makeChecker: TypeChecker[CS]
}