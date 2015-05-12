package incremental.pcf.with_records

import incremental.Node._
import incremental.pcf.Exp_$
import incremental.{Node_, SyntaxChecking}

/**
 * Created by seba on 15/11/14.
 */
case object Record extends Exp_(_ => RecordSyntax)
case object Project extends Exp_(simple(Seq(classOf[Symbol]), Exp_.cExp))


object RecordSyntax extends SyntaxChecking.SyntaxChecker(Record) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (lits.exists(!_.isInstanceOf[Symbol]))
      error(s"All literals must be record labels of type Symbol, but found ${lits.filter(!_.isInstanceOf[Symbol])}")

    if (kids.exists(!_.kind.isInstanceOf[Exp_]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp_])}")

    if (lits.size != kids.size)
      error(s"Mismatching number of record labels (${lits.size}}) and initializing expressions (${kids.size}})")
  }
}