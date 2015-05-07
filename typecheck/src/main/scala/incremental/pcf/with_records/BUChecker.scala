package incremental.pcf.with_records

import constraints.equality.{Type, ConstraintSystemFactory, EqConstraint, ConstraintSystem}
import incremental.Node_
import incremental.pcf
import incremental.pcf.TypeCheckerFactory

/**
* Created by seba on 15/11/14.
*/
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends pcf.BUChecker[CS] {

  override def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case Record =>
      val keys = e.lits.asInstanceOf[Seq[Symbol]]

      var fieldtypes = Map[Symbol,Type]()
      var kidreqs = Seq[Reqs]()

      for (i <- 0 until e.kids.seq.size) {
        val kidsymbol = keys(i)
        val (t,r,_) = e.kids.seq(i).typ
        fieldtypes = fieldtypes + (kidsymbol -> t)
        kidreqs = kidreqs :+ r
      }

      var (mcons, mreqs) = mergeReqMaps(kidreqs)
      (TRecord(fieldtypes), mreqs, mcons)

    case Project =>
      val label = e.lits(0).asInstanceOf[Symbol]
      val (t1, reqs, _) = e.kids(0).typ
      val X = freshUVar()
      (X, reqs, Seq(EqRecordProjectConstraint(t1, label, X)))

    case _ => super.typecheckStep(e)
  }
}

case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}