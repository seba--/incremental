package incremental.pcf.with_records

import constraints.equality._
import incremental.Node._
import incremental.pcf.Exp
import incremental.pcf.PCFCheck._
import incremental.{Context, Node_, SyntaxChecking}

/**
 * Created by seba on 15/11/14.
 */
case object Record extends Exp(_ => RecordSyntax){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val keys = lits.asInstanceOf[Seq[Symbol]]

    var fieldtypes = Map[Symbol,Type]()
    var kidreqs = Seq[Reqs]()

    for (i <- 0 until kids.seq.size) {
      val kidsymbol = keys(i)
      val (t,r) = kids.seq(i).typ
      fieldtypes = fieldtypes + (kidsymbol -> t)
      kidreqs = kidreqs :+ r
    }

    var (mcons, mreqs) = mergeReqMaps(kidreqs)

    context.addConstraintSeq(mcons)

    (TRecord(fieldtypes), mreqs)
  }
}
case object Project extends Exp(simple(Seq(classOf[Symbol]), Exp.cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val label = lits(0).asInstanceOf[Symbol]
    val (t1, reqs) = kids(0).typ
    val X = freshUVar()
    context.addConstraint(EqRecordProjectConstraint(t1, label, X))
    (X, reqs)
  }
}


object RecordSyntax extends SyntaxChecking.SyntaxChecker(Record) {
  def check[C, CS, T](lits: Seq[Lit], kids: Seq[Node_[C, CS, T]]) {
    if (lits.exists(!_.isInstanceOf[Symbol]))
      error(s"All literals must be record labels of type Symbol, but found ${lits.filter(!_.isInstanceOf[Symbol])}")

    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")

    if (lits.size != kids.size)
      error(s"Mismatching number of record labels (${lits.size}}) and initializing expressions (${kids.size}})")
  }
}