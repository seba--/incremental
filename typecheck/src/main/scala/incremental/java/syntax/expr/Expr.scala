package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.JavaContext
import incremental.java.syntax._
import incremental.{NodeKind, SyntaxChecking}
import incremental.java.syntax.JavaSyntaxChecker._
import incremental.Context

/**
 * Created by qwert on 27.03.15.
 */
abstract class Expr(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[Constraint, Result](syntaxcheck) with NT_ElemVal with NT_VarInit
object Expr {
  val cExpr = classOf[Expr]
}
import incremental.java.syntax.expr.Expr._

// Cast Operators
case object CastPrim extends Expr(simple(Seq(classOf[PrimType]), cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    //val t1: PrimType = lits(0)
    val t1 = lits(0).asInstanceOf[PrimType]
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(0).typ

    val t1IsNumerical = OneOf(t1, numTypes)
    val t2IsNumerical = OneOf(t2, numTypes)

    context.addConstraints(t1IsNumerical, t2IsNumerical)

    (ExprType(t1), vReqs2, cReqs2)
  }
}
case object CastRef extends Expr(simple(Seq(classOf[RefType]), cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object InstanceOf extends Expr(simple(Seq(classOf[RefType]), cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

// Class Instance Creation
case object NewInstance extends Expr((lits(Seq(classOf[TypeArgs], classOf[ClassOrInterfaceType])) andAlso exprKids) orElse
                                     (lits(Seq(classOf[ClassOrInterfaceType])) andAlso exprKids)){ // TODO: ClassBody? (orElse)
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object QNewInstance extends Expr((lits(Seq(classOf[String])) orElse lits(Seq(classOf[TypeArgs], classOf[String])) orElse
                                       lits(Seq(classOf[String], classOf[TypeArgs])) orElse lits(Seq(classOf[TypeArgs], classOf[String], classOf[TypeArgs])))
                                      andAlso
                                       (exprKids)){ //TODO: orElse manyFollowedByOne(cExpr, classOf[ClassBody])))
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}