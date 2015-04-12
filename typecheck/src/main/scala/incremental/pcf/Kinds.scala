package incremental.pcf

import incremental.Node._
import incremental.NodeKind
import incremental.SyntaxChecking
import incremental.Typ

/**
 * Created by seba on 13/11/14.
 */

<<<<<<< HEAD
case object Num extends ExpKind(0)
case object Add extends ExpKind(2)
case object Mul extends ExpKind(2)
case object Var extends ExpKind(0)
case object Abs extends ExpKind(1)
case object App extends ExpKind(2)
case object If0 extends ExpKind(3)
case object Fix extends ExpKind(1)
case object Field extends ExpKind(2)
=======
abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Num extends Exp(simple(Seq(classOf[Integer])))
case object Add extends Exp(simple(cExp, cExp))
case object Mul extends Exp(simple(cExp, cExp))
case object Var extends Exp(simple(Seq(classOf[Symbol])))
case object Abs extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[Typ[_]]), cExp))
case object App extends Exp(simple(cExp, cExp))
case object If0 extends Exp(simple(cExp, cExp, cExp))
case object Fix extends Exp(simple(cExp))
>>>>>>> origin/master
