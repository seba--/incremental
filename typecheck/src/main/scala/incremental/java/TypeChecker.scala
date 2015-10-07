package incremental.java

import constraints.javacons._
import incremental.MyBuilder
import incremental.java.syntax.{UVar, Type}

import scala.collection.generic.CanBuildFrom

/**
 * Created by qwert on 22.07.15.
 */

abstract class TypeChecker[CS <: ConstraintSystem[CS]] extends incremental.TypeChecker[Gen, Constraint, CS]{
  type T = Type
  type CSFactory <: ConstraintSystemFactory[CS]
  implicit val csFactory: CSFactory

  implicit def bf[K, V] = new CanBuildFrom[Iterable[(K, V)], ((K, V), V), Map[K, V]] {
    def apply = new MyBuilder
    def apply(from: Iterable[(K, V)]) = from.foldLeft(new MyBuilder[K, V])((b, p) => b += ((p) -> p._2))
  }

  def freshUVar() = UVar(freshSymbol("x$"))
}

trait TypeCheckerFactory[CS <: ConstraintSystem[CS]] {
  def makeChecker: TypeChecker[CS]
}