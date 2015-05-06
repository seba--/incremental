package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, Node_, SyntaxChecking}

/**
 * Created by qwert on 29.04.15.
 */
object ArrayInitSyntax extends SyntaxChecking.SyntaxChecker(ArrayInitialize){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    for(kid <- kids)
      if(!classOf[Expr].isInstance(kid.kind) || !classOf[ArrayInit].isInstance(kid.kind))
        error(s"All kids must be of kind Expr or ArrayInit, but found ${kid.kind.getClass}")

    if(kids.exists(!_.kind.isInstanceOf[Expr]))
      error(s"All kids must be of kind Expr, but found ${kids.filter(!_.kind.isInstanceOf[Expr])}")

    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

object ArrayCreationSyntax extends SyntaxChecking.SyntaxChecker(NewArray){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if(lits.exists(_.isInstanceOf[ArrayBaseType]))
      error(s"All literals must be of kind ArrayBaseType, but found ${lits.filter(!_.isInstanceOf[ArrayBaseType])}")

    if(lits.size != 1)
      error(s"There must only be one array base type, but found ${lits.size}")

    if(!lits(0).isInstanceOf[ArrayBaseType])
        error(s"Only allowed ArrayBaseType as type annotation, but found ${lits(1)}")

    if(kids.exists(!_.kind.isInstanceOf[Dimension]))
      error(s"All kids must be of kind Dimension, but found ${kids.filter(!_.kind.isInstanceOf[Dimension])}")
  }
}

object BlockSyntax extends SyntaxChecking.SyntaxChecker(Block){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // stm syntax andAlso nolits syntax
    if(kids.exists(!_.kind.isInstanceOf[Stm]))
      error(s"All kids must be of kind Stm, but found ${kids.filter(!_.kind.isInstanceOf[Stm])}")

    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")
  }
}

object FieldDecSyntax extends SyntaxChecking.SyntaxChecker(FieldDec){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    // TODO
  }
}

object ArrayVarDecSyntax extends SyntaxChecking.SyntaxChecker(FieldDec){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 1)
      error(s"Just one literal allowed, but found ${lits.size}")

    if(!classOf[String].isInstance(lits(0)))
      error(s"The first literal must be an identifier, but was ${lits(0).getClass}")

    if(kids.size == 0)
      error(s"Kids must not be empty")

    if(kids.exists(!_.kind.isInstanceOf[Dimension]))
      error(s"All kids must be of kind Dim, but found ${kids.filter(!_.kind.isInstanceOf[Dimension])}")
  }
}

object ArrayVarDecInitSyntax extends SyntaxChecking.SyntaxChecker(FieldDec){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 1)
      error(s"Just one literal allowed, but found ${lits.size}")

    if(!classOf[String].isInstance(lits(0)))
      error(s"The first literal must be an identifier, but was ${lits(0).getClass}")

    if(kids.size == 0)
      error(s"Kids must not be empty")

    for(i <- 0 until kids.size-1)
      if(classOf[Dimension].isInstance(kids(i).kind))
        error(s"All kids but the last must be of kind Dim, but found ${kids(i).kind.getClass}")

    if(!classOf[Expr].isInstance(kids(kids.size-1)) || !classOf[ArrayInit].isInstance(kids(kids.size-1)))
      error(s"The last kid must be of kind Expr or ArrayInit, but was ${kids(kids.size-1).getClass}")
  }
}

object MethodInvokationSyntax extends SyntaxChecking.SyntaxChecker(Invoke) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")

    if(kids.size < 1)
      error(s"At least 1 kid needed")

    if(!kids(0).kind.isInstanceOf[MethodSpec])
      error(s"First kind must be MethodSpec, but was ${kids(0).kind.getClass}")

    if(kids.size >= 2)
      for(i <- 1 until kids.size)
        if(!classOf[Expr].isInstance(kids(i).kind))
          error(s"All argument kinds must be of kind Expr, but found ${kids(i).kind.getClass}")
  }
}

// TODO: how to integrate this into Expr and Stm syntax or Node, like simple()
case class ExprKindsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.exists(!_.kind.isInstanceOf[Expr]))
      error(s"All kids must be of kind Expr, but found ${kids.filter(!_.kind.isInstanceOf[Expr])}")
  }
}

case class StmKindsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(kids.exists(!_.kind.isInstanceOf[Stm]))
      error(s"All kids must be of kind Stm, but found ${kids.filter(!_.kind.isInstanceOf[Stm])}")
  }
}

case class ExprOrStmKindsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    for(kid <- kids)
      if(!classOf[Expr].isInstance(kid.kind) || !classOf[Stm].isInstance(kid.kind))
        error(s"All kids must be of kind Stm or Expr, but found ${kid.kind.getClass}")
  }
}

case class NoLitsSyntax(k: NodeKind) extends SyntaxChecking.SyntaxChecker(k) {
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]): Unit = {
    if(lits.size != 0)
      error(s"No literals allowed, but found ${lits.size}")
  }
}