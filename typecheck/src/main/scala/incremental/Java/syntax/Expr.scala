package incremental.Java.syntax

import incremental.Java.JavaCheck._
import incremental.Node._
import incremental.{Node_, NodeKind, SyntaxChecking}
import JavaSyntaxChecker._

/**
 * Created by qwert on 27.03.15.
 */
abstract class Expr(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[StepResult](syntaxcheck) with NT_ElemVal with NT_VarInit
object Expr {
  val cExpr = classOf[Expr]
}
import Expr._

// Literals
case object Lit extends Expr(simple(Seq(classOf[Literal]))){
  def typeOfInt(s: String): Type = if (s.endsWith("l") || s.endsWith("L")) TLong() else TInt()
  def typeOfFloat(s: String): Type = if (s.endsWith("f") || s.endsWith("F")) TFloat() else TDouble()

  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = lits(0) match {
    case Deci(s)     => (ExprType(typeOfInt(s)), emptyCReqs, emptyVReqs, emptyCons)
    case Hexa(s)     => (ExprType(typeOfInt(s)), emptyCReqs, emptyVReqs, emptyCons)
    case Octa(s)     => (ExprType(typeOfInt(s)), emptyCReqs, emptyVReqs, emptyCons)
    case Float(s)    => (ExprType(typeOfFloat(s)), emptyCReqs, emptyVReqs, emptyCons)
    case Bool(b)     => (ExprType(TBoolean()), emptyCReqs, emptyVReqs, emptyCons)
    case Char(s)     => (ExprType(TChar()), emptyCReqs, emptyVReqs, emptyCons)
    case StringL(s)  => (ExprType(TString), emptyCReqs, emptyVReqs, emptyCons)
    case Null()      => (ExprType(???), emptyCReqs, emptyVReqs, emptyCons) // TODO: new type var (ref) for result type
    case ClassL(t)   => (ExprType(t), emptyCReqs, emptyVReqs, emptyCons)
    case VoidClass() => ???
  }
}

//abstract class Literal(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Expr(syntaxcheck)
//case object Deci extends Literal(simple(Seq(classOf[String])))
//case object Hexa extends Literal(simple(Seq(classOf[String])))
//case object Octa extends Literal(simple(Seq(classOf[String])))
//case object Float extends Literal(simple(Seq(classOf[String])))
//case object Bool extends Literal(simple(Seq(classOf[BoolLiteral])))
//case object Char extends Literal(simple(Seq(classOf[String])))
//case object String extends Literal(simple(Seq(classOf[String])))
//case object Null extends Literal(simple())
trait Literal
case class Deci(s: String) extends Literal
case class Hexa(s: String) extends Literal
case class Octa(s: String) extends Literal
case class Float(s: String) extends Literal
case class Bool(b: BoolLiteral) extends Literal
case class Char(s: String) extends Literal
case class StringL(s: String) extends Literal // name conflict with java.lang.String
case class Null() extends Literal
trait BoolLiteral
case class True() extends BoolLiteral
case class False() extends BoolLiteral
trait ClassLiteral extends Literal
case class ClassL(t: Type) extends ClassLiteral // name conflict with Class type of scala
case class VoidClass() extends ClassLiteral

// this reference
case object This extends Expr(simple())
case object QThis extends Expr(simple(Seq(classOf[TypeName])))

// Comparison Operators
case object Gt extends Expr(simple(cExpr, cExpr))
case object GtEq extends Expr(simple(cExpr, cExpr))
case object Lt extends Expr(simple(cExpr, cExpr))
case object LtEq extends Expr(simple(cExpr, cExpr))
case object Eq extends Expr(simple(cExpr, cExpr))
case object NotEq extends Expr(simple(cExpr, cExpr))

// Arithmetic Operators
case object Plus extends Expr(simple(cExpr, cExpr) orElse (simple(cExpr)))
case object Minus extends Expr(simple(cExpr, cExpr) orElse (simple(cExpr)))
case object Mul extends Expr(simple(cExpr, cExpr))
case object Div extends Expr(simple(cExpr, cExpr))
case object Remain extends Expr(simple(cExpr, cExpr))
case object PreIncr extends Expr(simple(cExpr))
case object PostIncr extends Expr(simple(cExpr))
case object PreDecr extends Expr(simple(cExpr))
case object PostDecr extends Expr(simple(cExpr))
case object LeftShift extends Expr(simple(cExpr, cExpr))
case object RightShift extends Expr(simple(cExpr, cExpr))
case object URightShift extends Expr(simple(cExpr, cExpr))

// Bitwise Operators
case object Complement extends Expr(simple(cExpr))
case object And extends Expr(simple(cExpr, cExpr))
case object Or extends Expr(simple(cExpr, cExpr))
case object ExcOr extends Expr(simple(cExpr, cExpr))

// Logical Operators
case object Not extends Expr(simple(cExpr))
case object LazyAnd extends Expr(simple(cExpr, cExpr))
case object LazyOr extends Expr(simple(cExpr, cExpr))

// Conditional Operator (Expr ? Expr : Expr)
case object Cond extends Expr(simple(cExpr, cExpr, cExpr))

// Cast Operators
case object CastPrim extends Expr(simple(Seq(classOf[PrimType]), cExpr))
case object CastRef extends Expr(simple(Seq(classOf[RefType]), cExpr))
case object InstanceOf extends Expr(simple(Seq(classOf[RefType]), cExpr))

// Assignment Operators
case object Assign extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr))
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