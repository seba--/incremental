package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{Context, NodeKind, SyntaxChecking}
import incremental.java.syntax.expr.Expr._

/**
 * Created by qwert on 29.09.15.
 */

// Field Access
abstract class FieldAccess(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Expr(syntaxcheck)
case object Field extends FieldAccess(simple(Seq(classOf[String]), cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object SuperField extends FieldAccess(simple(Seq(classOf[String]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object QSuperField extends FieldAccess(simple(Seq(classOf[TypeName], classOf[String]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}