package incremental.pcf.let_poly

import incremental.Node._
import incremental.{NodeKind, Node_, SyntaxChecking}
import constraints.equality_letpoly._

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
case object ListL extends Exp(_ => ListLSyntax)

object ListLSyntax extends SyntaxChecking.SyntaxChecker(ListL) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if (kids.exists(!_.kind.isInstanceOf[Exp]))
      error(s"All kids must be of sort Exp, but found ${kids.filter(!_.kind.isInstanceOf[Exp])}")
  }
}

case object AppendL extends Exp(simple(cExp, cExp))

//  Exp(_ => AppendLSyntax)
//
//object AppendLSyntax extends SyntaxChecking.SyntaxChecker(AppendL) {
//  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
//    if (kids.size != 2 )
//      error(s"Extedted to have two kids but got ${kids.size}")
//    if (kids(0).kind != ListL)
//      error(s"Expected a list for the Append but got ${kids(0).kind}")
//  }
//}
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