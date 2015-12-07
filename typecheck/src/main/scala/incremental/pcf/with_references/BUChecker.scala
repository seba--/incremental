package incremental.pcf.with_references

import constraints.equality.{ConstraintSystemFactory, EqConstraint, ConstraintSystem}
import incremental.Node_
import incremental.pcf
import incremental.pcf.TypeCheckerFactory

/**
 * Created by seba on 15/11/14.
 */
trait BUChecker[CS <: ConstraintSystem[CS]] extends pcf.BUChecker[CS] {

}

case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}