package incremental.pcf

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}
import constraints.equality._

/**
 * Created by seba on 13/11/14.
 */

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Num extends Exp(simple(Seq(classOf[Integer])))
case object Add extends Exp(simple(cExp, cExp))
case object Mul extends Exp(simple(cExp, cExp))
case object Var extends Exp(simple(Seq(classOf[Symbol])))
case object Abs extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[Type]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp)))
case object App extends Exp(simple(cExp, cExp))
case object If0 extends Exp(simple(cExp, cExp, cExp))
case object Fix extends Exp(simple(cExp))
