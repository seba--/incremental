package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, Node_, SyntaxChecking}

/**
 * Created by qwert on 29.04.15.
 */
object ArrayInitSyntax extends SyntaxChecking.SyntaxChecker(ArrayInit){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if(kids.exists(!_.kind.isInstanceOf[Expr]))
      error(s"All kids must be of sort Expr, but found ${kids.filter(!_.kind.isInstanceOf[Expr])}")

    /*// TODO: needed?
    if(lits.size != 0)
      error(s"The number of literals must be zero, but was ${lits.size}")
      */
  }
}

object ArrayCreationSyntax extends SyntaxChecking.SyntaxChecker(NewArray){
  def check[T](lits: Seq[Lit], kids: Seq[Node_[T]]) {
    if(lits.exists(_.isInstanceOf[ArrayBaseType]))
      error(s"All literals must be of type ArrayBaseType, but found ${lits.filter(!_.isInstanceOf[ArrayBaseType])}")

    if(lits.size != 1)
      error(s"There must only be one array base type, but found ${lits.size}")

    if(kids.exists(!_.kind.isInstanceOf[Dimension]))
      error(s"All kids must be of sort Dimension, but found ${kids.filter(!_.kind.isInstanceOf[Expr])}")
  }
}