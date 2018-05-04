package incremental.pcf.let_poly

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
case object Float extends Exp(simple(Seq(classOf[Integer])))
case object Char extends Exp(simple(Seq(classOf[Symbol])))
case object Bool extends Exp(simple(Seq(classOf[Symbol])))
case object isLower extends Exp(simple(cExp))
case object Add extends Exp(simple(cExp, cExp))
case object Mul extends Exp(simple(cExp, cExp))
case object Var extends Exp(simple(Seq(classOf[Symbol])))
case object Abs extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[Type]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp)))
case object App extends Exp(simple(cExp, cExp))
case object If0 extends Exp(simple(cExp, cExp, cExp))
case object Fix extends Exp(simple(cExp))


abstract class Decl(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Decl {
  val cDecl = classOf[Decl]
}
import Decl._

case object VarDec extends Decl(simple(Seq(classOf[Symbol]), cExp))
case object FunDec extends Decl(simple(Seq(classOf[Symbol], classOf[Symbol]), cExp))

case object Let extends Exp(simple(cDecl, cExp))

case object VarL extends Exp(simple(Seq(classOf[Symbol])))
case object LetV extends Exp(simple(Seq(classOf[Symbol]), cExp, cExp))