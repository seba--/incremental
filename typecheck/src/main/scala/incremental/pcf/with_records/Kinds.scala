package incremental.pcf.with_records

import incremental.Exp._
import incremental.{Exp_, SyntaxChecker, ExpKind}

/**
 * Created by seba on 15/11/14.
 */
case object Record extends ExpKind(_ => RecordSyntax)
case object Project extends ExpKind


object RecordSyntax extends SyntaxChecker(Record) {
  def check[T](lits: Seq[Lit], kids: Seq[Exp_[T]]) {
    if (lits.exists(!_.isInstanceOf[Symbol]))
      error(s"All literals must be record labels of type Symbol, but found ${lits.filter(!_.isInstanceOf[Symbol])}")

    if (lits.size != kids.size)
      error(s"Mismatching number of record labels (${lits.size}}) and initializing expressions (${kids.size}})")
  }
}