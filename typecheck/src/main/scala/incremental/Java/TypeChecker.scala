package incremental.Java

import constraints.javacons._
import incremental.Java.syntax.{UVar, Type}

/**
 * Created by qwert on 22.07.15.
 */

abstract class TypeChecker[CS <: ConstraintSystem[CS]] extends incremental.TypeChecker[Gen, Constraint, CS]{
  type T = Type
  //type CSFactory <: ConstraintSystemFactory[CS]
  //implicit val csFactory: CSFactory

  def freshUVar() = UVar(freshSymbol("x$"))
}