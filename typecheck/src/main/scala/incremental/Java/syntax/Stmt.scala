package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}
import incremental.Java.syntax.JavaSyntaxChecker._

/**
 * Created by qwert on 05.04.15.
 */


abstract class Stm(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Stm {
  val cStm = classOf[Stm]
}
import Stm._
import Expr._

case object Empty extends Stm(simple())
case object ExprStm extends Stm(simple(cExpr))

case object If extends Stm(simple(Seq(cExpr, cStm)))
case object IfElse extends Stm(simple(Seq(cExpr, cStm, cStm)))

abstract class Block(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Stm(syntaxcheck)
case object Block extends Block(_ => BlockSyntax)

/*case object LocalVarDec() extends Stm // TODO local var dec/var dec

case object While extends Stm(simple(Seq(cExpr, cStm)))
case object DoWhile extends Stm(simple(Seq(cStm, cExpr)))

case object For(init: Expr, cond: Expr, update: Seq[Expr], body: Stm) extends Stm // TODO: init is var dec

case object Continue extends Stm(simple()) // TODO: label with orElse
case object Break extends Stm(simple())*/


//////////////
// LocalVariableDeclarations
trait NT_LocalVarDecStm
trait NT_LocalVarDec

case object LocalVarDecStm extends NodeKind(simple(LocalVarDec.getClass)) with NT_LocalVarDecStm
case object LocalVarDec extends NodeKind(litsFollowedBy(classOf[VarMod], classOf[Type]) andAlso unsafeAllKids(classOf[NT_Anno], classOf[NT_VarDec]) andAlso nonEmptyKids) with NT_LocalVarDec