//package cocogen.pcf
//
//import constraints.equality._
//import incremental.MyBuilder
//import incremental.pcf.UVar
//
//import scala.collection.generic.CanBuildFrom
//
///**
// * Created by seba on 13/11/14.
// */
//abstract class EnumeratorBase[CS <: ConstraintSystem[CS]] extends incremental.TypeChecker[Gen, Constraint, CS] {
//  type T = Type
//  type CSFactory <: ConstraintSystemFactory[CS]
//  implicit val csFactory: CSFactory
//
//  implicit def bf[K,V] = new CanBuildFrom[Iterable[(K,V)], ((K, V), V), Map[K, V]] {
//    def apply = new MyBuilder
//    def apply(from: Iterable[(K,V)]) = from.foldLeft(new MyBuilder[K,V])((b, p) => b += ((p) -> p._2))
//  }
//
//  def freshUVar() = UVar(freshSymbol("x$"))
//}
//
//trait EnumeratorBaseFactory[CS <: ConstraintSystem[CS]] {
//  def makeChecker: EnumeratorBase[CS]
//}