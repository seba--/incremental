package incremental.Java

/**
 * Created by qwert on 05.04.15.
 */
trait Stm {

}

case class Empty() extends Stm
case class ExprStm(e: Expr) extends Stm

case class If(cond: Expr, body: Stm) extends Stm
case class IfElse(cond: Expr, thenStm: Stm, elseStm: Stm) extends Stm

case class Block(body: Seq[Stm]) extends Stm

case class LocalVarDec() extends Stm // TODO local var dec/var dec

case class While(cond: Expr, body: Stm) extends Stm
case class DoWhile(body: Stm, cond: Expr) extends Stm

case class For(init: Expr, cond: Expr, update: Seq[Expr], body: Stm) extends Stm // TODO: init is var dec

case class Continue() extends Stm // TODO: label?
case class Break() extends Stm