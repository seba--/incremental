package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{NodeKind, SyntaxChecking}

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

  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = lits(0) match {
    case Deci(s)     => (ExprType(typeOfInt(s)), emptyVReqs, emptyCReqs, emptyCons)
    case Hexa(s)     => (ExprType(typeOfInt(s)), emptyVReqs, emptyCReqs, emptyCons)
    case Octa(s)     => (ExprType(typeOfInt(s)), emptyVReqs, emptyCReqs, emptyCons)
    case Float(s)    => (ExprType(typeOfFloat(s)), emptyVReqs, emptyCReqs, emptyCons)
    case Bool(b)     => (ExprType(TBoolean()), emptyVReqs, emptyCReqs, emptyCons)
    case Char(s)     => (ExprType(TChar()), emptyVReqs, emptyCReqs, emptyCons)
    case StringL(s)  => (ExprType(TString), emptyVReqs, emptyCReqs, emptyCons)
    case Null()      => (ExprType(freshUVar()), emptyVReqs, emptyCReqs, emptyCons)
    case ClassL(t)   => (ExprType(t), emptyVReqs, emptyCReqs, emptyCons)
    case VoidClass() => ???
  }
}

// this reference
case object This extends Expr(simple()){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val X = freshUVar()
    (ExprType(X), Map('this -> X), emptyCReqs, emptyCons)
  }
}
case object QThis extends Expr(simple(Seq(classOf[TypeName]))){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}