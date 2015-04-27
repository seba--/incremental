package incremental.pcf

import constraints.equality._
import incremental.Node.Node

/**
 * Created by seba on 13/11/14.
 */
abstract class TypeChecker[CS <: ConstraintSystem[CS]] extends incremental.TypeChecker[Type, UVar, EqConstraint, CS] {
  type CSFactory <: ConstraintSystemFactory[CS]
  implicit val csFactory: CSFactory
}

trait TypeCheckerFactory[CS <: ConstraintSystem[CS]] {
  def makeChecker: TypeChecker[CS]
}