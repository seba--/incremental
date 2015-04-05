package incremental.Java

/**
 * Created by qwert on 27.03.15.
 */
trait Expr {

}

// Literals
// TODO: leave string or put specific types where possible?
case class Deci(s: String) extends Expr
case class Hexa(s: String) extends Expr
case class Octa(s: String) extends Expr
case class Float(s: String) extends Expr
case class Bool(s: String) extends Expr
case class Char(s: String) extends Expr
case class String(s: String) extends Expr
case class This() extends Expr
case class Null() extends Expr

// Comparison Operators
case class Gt(lhs: Expr, rhs: Expr) extends Expr
case class GtEq(lhs: Expr, rhs: Expr) extends Expr
case class Lt(lhs: Expr, rhs: Expr) extends Expr
case class LtEq(lhs: Expr, rhs: Expr) extends Expr
case class Eq(lhs: Expr, rhs: Expr) extends Expr
case class NotEq(lhs: Expr, rhs: Expr) extends Expr

// Arithmetic Operators
case class UnaryPlus(e: Expr) extends Expr // TODO: overloading?
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
case class PrimCast(T: Type, e: Expr) extends Expr

// Assignment Operators
case class Assign(lhs: Name, rhs: Expr) extends Expr // TODO: lhs must be name?

// Arrays
case class ArrayAccess(lhs: Expr, rhs: Expr) extends Expr
case class ArrayInit(initlist: Seq[Expr]) extends Expr
case class NewArray(T: Type, dimlist: Seq[Expr], e: Expr) extends Expr

// Field Access
case class Field(e: Expr, x: Name) extends Expr // TODO: Name must be simple name
case class SuperField(x: Name) extends Expr // TODO: Name must be simple name
