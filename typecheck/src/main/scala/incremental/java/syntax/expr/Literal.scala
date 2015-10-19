package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{Context, NodeKind, SyntaxChecking}

/**
 * Created by qwert on 28.09.15.
 */

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

// Literals
case object Lit extends Expr(simple(Seq(classOf[Literal]))){
  def typeOfInt(s: String): Type = if (s.endsWith("l") || s.endsWith("L")) TLong() else TInt()
  def typeOfFloat(s: String): Type = if (s.endsWith("f") || s.endsWith("F")) TFloat() else TDouble()

  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = lits(0) match {
    case Deci(s)     => (ExprType(typeOfInt(s)), emptyVReqs, emptyCReqs)
    case Hexa(s)     => (ExprType(typeOfInt(s)), emptyVReqs, emptyCReqs)
    case Octa(s)     => (ExprType(typeOfInt(s)), emptyVReqs, emptyCReqs)
    case Float(s)    => (ExprType(typeOfFloat(s)), emptyVReqs, emptyCReqs)
    case Bool(b)     => (ExprType(TBoolean()), emptyVReqs, emptyCReqs)
    case Char(s)     => (ExprType(TChar()), emptyVReqs, emptyCReqs)
    case StringL(s)  => (ExprType(TString), emptyVReqs, emptyCReqs)
    case Null()      => (ExprType(freshUVar()), emptyVReqs, emptyCReqs)
    case ClassL(t)   => (ExprType(t), emptyVReqs, emptyCReqs)
    case VoidClass() => ???
  }
}

// this reference
case object This extends Expr(simple()){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val X = freshUVar()
    (ExprType(X), Map('this -> X), emptyCReqs)
  }
}
case object QThis extends Expr(simple(Seq(classOf[TypeName]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}