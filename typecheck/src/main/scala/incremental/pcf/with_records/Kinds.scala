package incremental.pcf.with_records

import incremental.Exp._
import incremental.{SyntaxChecker, ExpKind}

/**
 * Created by seba on 15/11/14.
 */
case object Record extends ExpKind(RecordSyntax)
case object Project extends ExpKind


object RecordSyntax extends SyntaxChecker {
  def apply(lits: Seq[Lit], kids: Seq[Exp]): ErrorOption = {
    if (lits.exists(!_.isInstanceOf[Symbol]))
      return Some(s"All literals must be record labels of type Symbol, but found ${lits.filter(!_.isInstanceOf[Symbol])}")

    if (lits.size != kids.size)
      return Some(s"Mismatching number of record labels (${lits.size}}) and initializing expressions (${kids.size}})")

    None
  }
}