package incremental.pcf.with_references

import constraints.equality.{ConstraintSystemFactory, EqConstraint, ConstraintSystem}
import incremental.Node_
import incremental.pcf
import incremental.pcf.TypeCheckerFactory

/**
 * Created by seba on 15/11/14.
 */
trait BUChecker[CS <: ConstraintSystem[CS]] extends pcf.BUChecker[CS] {

  override def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case Ref =>
      val (t, reqs, _) = e.kids(0).typ
      (TRef(t), reqs, scala.Seq())
    case Deref =>
      val (t, reqs, _) = e.kids(0).typ
      val X = freshUVar()
      (X, reqs, scala.Seq(EqConstraint(TRef(X), t)))
    case Assign =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      (TUnit, mreqs, mcons :+ EqConstraint(t1, TRef(t2)))
    case Seq =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      (t2, mreqs, mcons :+ EqConstraint(TUnit, t1))
    case _ => super.typecheckStep(e)
  }
}

case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}