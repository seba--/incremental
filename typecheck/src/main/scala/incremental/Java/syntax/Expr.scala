package incremental.Java.syntax

import incremental.Node._
import incremental.{NodeKind, SyntaxChecking}


/**
 * Created by qwert on 27.03.15.
 */
abstract class Expr(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object Expr {
  val cExpr = classOf[Expr]
}
import Expr._

// Literals
//case class Lit(l: Literal) extends Expr{
//  def typecheck() = l.typecheck()
//}
abstract class Literal(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Expr(syntaxcheck)
case object DeciLit extends Literal(simple(Seq(classOf[String])))
case object HexaLit extends Literal(simple(Seq(classOf[String])))
case object OctaLit extends Literal(simple(Seq(classOf[String])))
case object FloatLit extends Literal(simple(Seq(classOf[String])))
case object BoolLit extends Literal(simple(Seq(classOf[String])))
case object CharLit extends Literal(simple(Seq(classOf[String])))
case object StringLit extends Literal(simple(Seq(classOf[String])))
case object NullLit extends Literal(simple())

abstract class ClassLiteral(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Literal(syntaxcheck)
case object Class extends ClassLiteral(simple(Seq(classOf[Type])))
case object VoidClass extends ClassLiteral(simple())

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
case object CastPrim extends Expr(simple(Seq(classOf[PrimType], cExpr)))
case object CastRef extends Expr(simple(Seq(classOf[RefType], cExpr)))
case object InstanceOf extends Expr(simple(Seq(cExpr, classOf[RefType]))) // TODO: reihenfolge?

// Assignment Operators
trait LHS{} //TODO: extends ExprName with ArrayAccess with FieldAccess{}
case class Assign(lhs: LHS, rhs: Expr) extends Expr
case class AssignMul(lhs: LHS, rhs: Expr) extends Expr
case class AssignDiv(lhs: LHS, rhs: Expr) extends Expr
case class AssignRemain(lhs: LHS, rhs: Expr) extends Expr
case class AssignPlus(lhs: LHS, rhs: Expr) extends Expr
case class AssignMinus(lhs: LHS, rhs: Expr) extends Expr
case class AssignLeftShift(lhs: LHS, rhs: Expr) extends Expr
case class AssignRightShift(lhs: LHS, rhs: Expr) extends Expr
case class AssignURightShift(lhs: LHS, rhs: Expr) extends Expr
case class AssignAnd(lhs: LHS, rhs: Expr) extends Expr
case class AssignExcOr(lhs: LHS, rhs: Expr) extends Expr
case class AssignOr(lhs: LHS, rhs: Expr) extends Expr

// Array Access
case object ArrayAccess extends Expr(simple(cExpr, cExpr))

// Array Initialization
case object ArrayInit extends Expr(_ => ArrayInitSyntax) // TODO: test

// Array Creation
//abstract class ArrayCreationExpr(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Expr(syntaxcheck)
case object NewArray extends Expr(_ => ArrayCreationSyntax) // TODO: test

//case class NewArray1(t: ArrayBaseType, dimExprs: Seq[DimExpr], dims: Seq[Dim]) extends ArrayCreationExpr
//case class NewArray2(t: ArrayBaseType, dims: Seq[Dim], arrayInit: ArrayInit) extends ArrayCreationExpr

trait ArrayBaseType{} // TODO: extends PrimType with TypeName{}
case class UnboundWld(t: TypeName) extends ArrayBaseType

abstract class Dimension(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object Dim extends Dimension(simple())
case object DimExpr extends Dimension(simple(cExpr))

// Field Access
abstract class FieldAccess(syntaxcheck: SyntaxChecking.SyntaxCheck) extends Expr(syntaxcheck)
case object Field extends FieldAccess(simple(Seq(cExpr, classOf[String])))
// ExprName "." Id -> FieldAccess {reject}
case object SuperField extends FieldAccess(simple(Seq(classOf[String])))
case object QSuperField extends FieldAccess(simple(Seq(classOf[TypeName], classOf[String])))>

// Method Invocation
case object Invoke extends Expr(simple(Seq(classOf[MethodSpec], cExpr))) // TODO: methodInvoke syntax checker (like arrays)
//case object Invoke(methodSpec: MethodSpec, args: Seq[Expr]) extends Expr

abstract class MethodSpec(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
case object Method extends MethodSpec(simple(Seq(classOf[MethodName])) orElse simple(Seq(cExpr, classOf[String])) orElse simple(Seq(cExpr, classOf[TypeArgs], classOf[String])))
//case object Method1(name: MethodName) extends MethodSpec
//case object Method2(e: Expr, typeArgs: Option[TypeArgs], id: String) extends MethodSpec
case object SuperMethod extends MethodSpec(simple(Seq(classOf[String])) orElse simple(Seq(classOf[TypeArgs], classOf[String])))
//case object SuperMethod(typeArgs: Option[TypeArgs], id: String) extends MethodSpec
case object QSuperMethod extends MethodSpec(simple(Seq(classOf[TypeName], classOf[String])) orElse simple(Seq(classOf[TypeName], classOf[TypeArgs], classOf[String])))
//case object QSuperMethod(t: TypeName, typeArgs: Option[TypeArgs], id: String) extends MethodSpec
case object GenericMethod extends MethodSpec(simple(Seq(classOf[AmbName], classOf[TypeArgs], classOf[String])))

// Class Instance Creation

// Package Declaration
//case class PackageDec(annotation: Seq[Anno], name: PackageName) // TODO: extends?

// Import Declarations TODO: Move to another file
trait ImportDec{}
case class TypeImportDec(t: TypeName) extends ImportDec
case class TypeImportOnDemandDec(p: PackageName) extends ImportDec
case class StaticImportDec(t: TypeName, id: String) extends ImportDec
case class StaticImportOnDemandDec(t: TypeName) extends ImportDec