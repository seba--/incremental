package incremental.pcf.with_records

import incremental.Node._
import incremental.{Node_, SyntaxChecking, NodeKind}

/**
 * Created by seba on 15/11/14.
 */
case object Record extends NodeKind(_ => RecordSyntax)
case object Project extends NodeKind


object RecordSyntax extends SyntaxChecking.SyntaxChecker(Record) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (lits.exists(!_.isInstanceOf[Symbol]))
      error(s"All literals must be record labels of type Symbol, but found ${lits.filter(!_.isInstanceOf[Symbol])}")

    if (lits.size != kids.size)
      error(s"Mismatching number of record labels (${lits.size}}) and initializing expressions (${kids.size}})")
  }
}