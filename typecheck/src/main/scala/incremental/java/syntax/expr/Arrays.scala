package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{NodeKind, SyntaxChecking}
import incremental.java.syntax.expr.Expr._

/**
 * Created by qwert on 29.09.15.
 */

// Array Access
case object ArrayAccess extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}

// Array Creation
case object NewArray extends Expr(_ => ArrayCreationSyntax){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}

trait ArrayBaseType{} // extends PrimType with TypeName{}
case class UnboundWld(t: TypeName) extends ArrayBaseType

abstract class Dimension(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[StepResult](syntaxcheck){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}
case object Dim extends Dimension(simple())
case object DimExpr extends Dimension(simple(cExpr))