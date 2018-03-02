package incremental.haskell

import incremental.haskell.Node._
import incremental.haskell.SyntaxChecking.SyntaxCheck
import incremental.haskell.Exp._

import scala.collection.immutable.ListMap


class OptionKind(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object OptionKind {
  val cOpKind = classOf[OptionKind]

  def apply(syntaxcheck: SyntaxChecking.SyntaxCheck) = new OptionKind(syntaxcheck)

  def option(syntaxcheck: SyntaxChecking.SyntaxCheck): SyntaxChecking.SyntaxCheck = {
    case NNone => new SyntaxChecking.SyntaxChecker(NNone) {
      override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
        // fine
      }
      def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]): Unit = {
        // fine
      }
    }
    case NSome => new SyntaxChecking.SyntaxChecker(NSome) {
       def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]): Unit = {}
      override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
        if (lits.size != 0)
          error(s"Expected a child but got none")
        if (kids.seq.size != 1)
          error(s"Expected exactly one child but got ${kids.size}")
        val some = kids(0)
        syntaxcheck.apply(some.kind).check(some.lits, some.kids.seq)
      }
    }
    case k => new SyntaxChecking.SyntaxChecker(k) {
      override def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
        error(s"Wrong kind $k, expected OptionKind")
      }
      def checkC(lits: Seq[Lit], kids: Seq[SyntaxCheck]): Unit = {}
      }
  }
}

case object NNone extends OptionKind(simple())
case object NSome extends OptionKind(simple(cExp))



