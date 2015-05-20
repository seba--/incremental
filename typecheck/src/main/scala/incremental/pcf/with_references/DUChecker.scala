package incremental.pcf.with_references

import constraints.equality.{ConstraintSystemFactory, EqConstraint, ConstraintSystem}
import incremental.pcf.TypeCheckerFactory
import incremental.{Node_, pcf}

/**
 * Created by seba on 15/11/14.
 */
trait DUChecker[CS <: ConstraintSystem[CS]] extends pcf.DUChecker[CS] {

  override def typecheckStep(e: Node_[Result], ctx: TCtx): StepResult = e.kind match {
    case Ref =>
      val (t, cs) = typecheckRec(e.kids(0), ctx)
      (TRef(t), scala.Seq(), scala.Seq(cs))
    case Deref =>
      val (t1, cs) = typecheckRec(e.kids(0), ctx)
      val X = freshUVar()
      (X, scala.Seq(EqConstraint(TRef(X), t1)), scala.Seq(cs))
    case Assign =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      (TUnit, scala.Seq(EqConstraint(t1, TRef(t2))), scala.Seq(cs1, cs2))
    case Seq =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      (t2, scala.Seq(EqConstraint(TUnit, t1)), scala.Seq(cs1, cs2))
    case _ => super.typecheckStep(e, ctx)
  }
}

case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}