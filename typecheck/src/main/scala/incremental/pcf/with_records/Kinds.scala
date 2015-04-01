package incremental.pcf.with_records

import incremental.{SyntaxChecker, Syntax, ExpKind}

/**
 * Created by seba on 15/11/14.
 */
case object Record extends ExpKind(RecordSyntax)
case object Project extends ExpKind


object RecordSyntax extends SyntaxChecker {
  def apply(lits: Seq[Lit], kids: Seq[Exp]): Option[String] = {
    
  }
}