package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 27.03.15.
 */
abstract class Expr(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[StepResult](syntaxcheck) with NT_ElemVal with NT_VarInit
object Expr {
  val cExpr = classOf[Expr]
}
import incremental.java.syntax.expr.Expr._

// Cast Operators
case object CastPrim extends Expr(simple(Seq(classOf[PrimType]), cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    //val t1: PrimType = lits(0)
    val t1 = lits(0).asInstanceOf[PrimType]
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(0).typ

    val t1IsNumerical = OneOf(t1, numTypes)
    val t2IsNumerical = OneOf(t2, numTypes)

    (ExprType(t1), vReqs2, cReqs2, t1IsNumerical +: t2IsNumerical +: cons2)
  }
}
case object CastRef extends Expr(simple(Seq(classOf[RefType]), cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}
case object InstanceOf extends Expr(simple(Seq(classOf[RefType]), cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}

// Assignment Operators
case object Assign extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}
case object AssignMul extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignDiv extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignRemain extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignPlus extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignMinus extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignLeftShift extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignRightShift extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignURightShift extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignAnd extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignExcOr extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
case object AssignOr extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))

// Array Access
case object ArrayAccess extends Expr(simple(cExpr, cExpr))

// Array Creation
case object NewArray extends Expr(_ => ArrayCreationSyntax)

trait ArrayBaseType{} // extends PrimType with TypeName{}
case class UnboundWld(t: TypeName) extends ArrayBaseType

abstract class Dimension(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object Dim extends Dimension(simple())
case object DimExpr extends Dimension(simple(cExpr))

// Field Access
abstract class FieldAccess(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Expr(syntaxcheck)
case object Field extends FieldAccess(simple(Seq(classOf[String]), cExpr))
case object SuperField extends FieldAccess(simple(Seq(classOf[String])))
case object QSuperField extends FieldAccess(simple(Seq(classOf[TypeName], classOf[String])))

// Method Invocation
case object Invoke extends Expr(_ => MethodInvokationSyntax)

abstract class MethodSpec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object Method extends MethodSpec(simple(Seq(classOf[MethodName])) orElse simple(Seq(classOf[String]), cExpr) orElse simple(Seq(classOf[TypeArgs], classOf[String]), cExpr))
case object SuperMethod extends MethodSpec(simple(Seq(classOf[String])) orElse simple(Seq(classOf[TypeArgs], classOf[String])))
case object QSuperMethod extends MethodSpec(simple(Seq(classOf[TypeName], classOf[String])) orElse simple(Seq(classOf[TypeName], classOf[TypeArgs], classOf[String])))
case object GenericMethod extends MethodSpec(simple(Seq(classOf[AmbName], classOf[TypeArgs], classOf[String])))

// Class Instance Creation
case object NewInstance extends Expr((lits(Seq(classOf[TypeArgs], classOf[ClassOrInterfaceType])) andAlso exprKids) orElse
                                     (lits(Seq(classOf[ClassOrInterfaceType])) andAlso exprKids)) // TODO: ClassBody? (orElse)
case object QNewInstance extends Expr((lits(Seq(classOf[String])) orElse lits(Seq(classOf[TypeArgs], classOf[String])) orElse
                                       lits(Seq(classOf[String], classOf[TypeArgs])) orElse lits(Seq(classOf[TypeArgs], classOf[String], classOf[TypeArgs])))
                                      andAlso
                                       (exprKids)) //TODO: orElse manyFollowedByOne(cExpr, classOf[ClassBody])))