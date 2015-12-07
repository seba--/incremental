package incremental.pcf.with_records

import constraints.equality._
import incremental.pcf.{PCFCheck, TypeCheckerFactory}
import incremental.{NodeKind, Node_, pcf}

/**
* Created by seba on 15/11/14.
*/
abstract class DUChecker[CS <: ConstraintSystem[CS]] extends pcf.DUChecker[CS] {

  override def typecheckStep(e: Node_[Constraint, CS, Result], ctx: TCtx): StepResult = e.kind.asInstanceOf[NodeKind[Constraint, PCFCheck.Result]] match {
    case Record =>
      val keys = e.lits.asInstanceOf[Seq[Symbol]]

      var fieldtypes = Map[Symbol,Type]()
      var kidsols = Seq[CS]()

      for (i <- 0 until e.kids.seq.size) {
        val kidsymbol = keys(i)
        val (t, cs) = typecheckRec(e.kids(i), ctx)
        fieldtypes = fieldtypes + (kidsymbol -> t)
        kidsols = kidsols :+ cs
      }

      (TRecord(fieldtypes), Seq(), kidsols)

    case Project =>
      val label = e.lits(0).asInstanceOf[Symbol]
      val (t1, cs) = typecheckRec(e.kids(0), ctx)
      val X = freshUVar()
      (X, Seq(EqRecordProjectConstraint(t1, label, X)), Seq(cs))

    case _ => super.typecheckStep(e, ctx)
  }
}

case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}