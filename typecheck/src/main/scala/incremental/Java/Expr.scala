package incremental.Java

import incremental.ConstraintOps.Solution

/**
 * Created by qwert on 27.03.15.
 */
trait Expr {
  type ClassReqs = List // TODO: specify element type, may need to split this
  type Bindings = Map[String, Type]
  type Result = (Type, Bindings, Bindings, ClassReqs, Solution)

  type TError = String // TODO: specify TError
  def typecheck(): Either[Type, TError]
}

// Literals
//case class Lit(l: Literal) extends Expr{
//  def typecheck() = l.typecheck()
//}
trait Literal extends Expr{}
case class DeciLit(s: String) extends Literal{
  def typecheck() = {
    if(s.endsWith("l") || s.endsWith("L"))
      Left(TLong())
    else
      Left(TInt())
  }
}
case class HexaLit(s: String) extends Literal{
  def typecheck() = {
    if(s.endsWith("l") || s.endsWith("L"))
      Left(TLong())
    else
      Left(TInt())
  }
}
case class OctaLit(s: String) extends Literal{
  def typecheck() = {
    if(s.endsWith("l") || s.endsWith("L"))
      Left(TLong())
    else
      Left(TInt())
  }
}
case class FloatLit(s: String) extends Literal{
  def typecheck() = {
    if(s.endsWith("f") || s.endsWith("F"))
      Left(TFloat())
    else
      Left(TDouble())
  }
}
case class BoolLit(s: String) extends Literal{
  def typecheck() = Left(TBoolean())
}
case class CharLit(s: String) extends Literal{
  def typecheck() = Left(TChar())
}
case class StringLit(s: String) extends Literal{
  def typecheck() = Left(ClassOrInterfaceType(TypeNameExt(PackageOrTypeNameExt(PackageOrTypeNameT("java"), "lang"), "String"), None))
}
case class NullLit() extends Literal

trait ClassLiteral extends Literal{}
case class Class(t: Type) extends ClassLiteral{
  def typecheck = Left(t)
}
case class VoidClass() extends ClassLiteral

case class This() extends Expr
case class QThis(t: TypeName) extends Expr

// Comparison Operators
case class Gt(lhs: Expr, rhs: Expr) extends Expr
case class GtEq(lhs: Expr, rhs: Expr) extends Expr
case class Lt(lhs: Expr, rhs: Expr) extends Expr
case class LtEq(lhs: Expr, rhs: Expr) extends Expr
case class Eq(lhs: Expr, rhs: Expr) extends Expr
case class NotEq(lhs: Expr, rhs: Expr) extends Expr

// Arithmetic Operators
case class UnaryPlus(e: Expr) extends Expr
case class Plus(lhs: Expr, rhs: Expr) extends Expr
case class UnaryMinus(e: Expr) extends Expr
case class Minus(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Div(lhs: Expr, rhs: Expr) extends Expr
case class Remain(lhs: Expr, rhs: Expr) extends Expr
case class PreIncr(e: Expr) extends Expr
case class PostIncr(e: Expr) extends Expr
case class PreDecr(e: Expr) extends Expr
case class PostDecr(e: Expr) extends Expr
case class LeftShift(lhs: Expr, rhs: Expr) extends Expr
case class RightShift(lhs: Expr, rhs: Expr) extends Expr
case class URightShift(lhs: Expr, rhs: Expr) extends Expr

// Bitwise Operators
case class Complement(e: Expr) extends Expr
case class And(lhs: Expr, rhs: Expr) extends Expr
case class Or(lhs: Expr, rhs: Expr) extends Expr
case class ExcOr(lhs: Expr, rhs: Expr) extends Expr

// Logical Operators
case class Not(e: Expr) extends Expr
case class LazyAnd(lhs: Expr, rhs: Expr) extends Expr
case class LazyOr(lhs: Expr, rhs: Expr) extends Expr

// Conditional Operator (Expr ? Expr : Expr)
case class Cond(ifExpr: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr

// Cast Operators
case class CastPrim(T: PrimType, e: Expr) extends Expr
case class CastRef(T: RefType, e: Expr) extends Expr
case class InstanceOf(e: Expr, T: RefType) extends Expr

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
trait ArrayAccess extends Expr with LHS{}
case class ArrayAccess1(arr: Expr, elem: Expr) extends ArrayAccess

// Array Initialization
case class ArrayInit(initList: Seq[Expr]) extends Expr

// Array Creation
trait ArrayCreationExpr extends Expr{}
case class NewArray1(t: ArrayBaseType, dimExprs: Seq[DimExpr], dims: Seq[Dim]) extends ArrayCreationExpr
case class NewArray2(t: ArrayBaseType, dims: Seq[Dim], arrayInit: ArrayInit) extends ArrayCreationExpr

trait ArrayBaseType{} // TODO: extends PrimType with TypeName{}
case class UnboundWld(t: TypeName) extends ArrayBaseType

case class Dim()
case class DimExpr(e: Expr)


// Field Access
trait FieldAccess extends Expr with LHS
case class Field(e: Expr, x: String) extends FieldAccess
// ExprName "." Id -> FieldAccess {reject}
case class SuperField(x: String) extends FieldAccess
case class QSuperField(t: TypeName, x: String) extends FieldAccess

// Method Invocation
case class Invoke(methodSpec: MethodSpec, args: Seq[Expr]) extends Expr

trait MethodSpec{}
case class Method1(name: MethodName) extends MethodSpec
case class Method2(e: Expr, typeArgs: Option[TypeArgs], id: String) extends MethodSpec
case class SuperMethod(typeArgs: Option[TypeArgs], id: String) extends MethodSpec
case class QSuperMethod(t: TypeName, typeArgs: Option[TypeArgs], id: String) extends MethodSpec
case class GenericMethod(name: AmbName, typeArgs: TypeArgs, id: String) extends MethodSpec

// Class Instance Creation

// Package Declaration
//case class PackageDec(annotation: Seq[Anno], name: PackageName) // TODO: extends?

// Import Declarations TODO: Move to another file
trait ImportDec{}
case class TypeImportDec(t: TypeName) extends ImportDec
case class TypeImportOnDemandDec(p: PackageName) extends ImportDec
case class StaticImportDec(t: TypeName, id: String) extends ImportDec
case class StaticImportOnDemandDec(t: TypeName) extends ImportDec