package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}
import incremental.Java.syntax.JavaSyntaxChecker._

/**
 * Created by qwert on 05.04.15.
 */


abstract class Stm(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck) with NT_BlockStm
object Stm {
  val cStm = classOf[Stm]
}
import Stm._
import Expr._

case object Empty extends Stm(simple())
case object Labeled extends Stm(simple(Seq(classOf[String]), cStm))
case object ExprStm extends Stm(simple(cExpr))

case object If extends Stm(simple(cExpr, cStm) orElse simple(cExpr, cStm, cStm))

case object AssertStm extends Stm(simple(cExpr) orElse simple(cExpr, cExpr))

// Switch
trait NT_SwitchBlock
trait NT_SwitchGroup
trait NT_SwitchLabel

case object Switch extends Stm(simple(cExpr, SwitchBlock.getClass))
case object SwitchBlock extends NodeKind(noLits andAlso unsafeAllKids(classOf[NT_SwitchGroup], classOf[NT_SwitchLabel])) with NT_SwitchBlock
case object SwitchGroup extends NodeKind(noLits andAlso nonEmptyKids andAlso unsafeAllKids(classOf[NT_SwitchLabel], classOf[NT_BlockStm])) with NT_SwitchGroup
case object Case extends NodeKind(simple(cExpr)) with NT_SwitchLabel
case object Default extends NodeKind(simple()) with NT_SwitchLabel

// Loops
case object While extends Stm(simple(cExpr, cStm))
case object DoWhile extends Stm(simple(cStm, cExpr))

case object For extends Stm((noLits andAlso uFollowedByKids(Seq(classOf[NT_LocalVarDec], cStm), cExpr)) orElse // first the statement then the increasing expressions
                            (noLits andAlso uFollowedByKids(Seq(classOf[NT_LocalVarDec], cExpr, cStm), cExpr))) // TODO: orElse: "for" "(" {Expr ","}* ";" Expr? ";" {Expr ","}* ")" Stm -> Stm {cons("For")}
case object ForEach extends Stm(simple(classOf[FormalParam], cExpr, cStm))

case object Break extends Stm(simple() orElse simple(Seq(classOf[String])))
case object Continue extends Stm(simple() orElse simple(Seq(classOf[String])))
case object Return extends Stm(simple() orElse simple(cExpr))
case object Throw extends Stm(simple(cExpr))

case object Synchronized extends Stm(simple(cExpr, Block.getClass))

// CatchClause
trait NT_CatchClause

case object Try extends Stm((noLits andAlso nonEmptyKids andAlso uFollowedByKids(classOf[NT_Block], classOf[NT_CatchClause])) orElse
                            (noLits andAlso uFollowedByKids(Seq(classOf[NT_Block], classOf[NT_Block]), classOf[NT_CatchClause])))
case object Catch extends NodeKind(simple(classOf[FormalParam], Block.getClass)) with NT_CatchClause

// Blocks
trait NT_BlockStm
trait NT_Block

case object Block extends Stm(noLits andAlso unsafeAllKids(classOf[NT_BlockStm])) with NT_Block
case object ClassDecStm extends NodeKind(noLits andAlso unsafeKids(Seq())) with NT_BlockStm // TODO: classOf[NT_ClassDec] in kids

// LocalVariableDeclarations
trait NT_LocalVarDecStm extends NT_BlockStm
trait NT_LocalVarDec

case object LocalVarDecStm extends NodeKind(simple(LocalVarDec.getClass)) with NT_LocalVarDecStm
case object LocalVarDec extends NodeKind(litsFollowedBy(classOf[VarMod], classOf[Type]) andAlso unsafeAllKids(classOf[NT_Anno], classOf[NT_VarDec]) andAlso nonEmptyKids) with NT_LocalVarDec