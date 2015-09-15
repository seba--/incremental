package incremental.Java.syntax

import incremental.Java.JavaCheck._
import incremental.Node._
import incremental.pcf.UVar
import incremental.{Node_, NodeKind, SyntaxChecking}
import JavaSyntaxChecker._
import constraints.javacons._
import incremental.Java.TypeChecker

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
case object This extends Expr(simple()){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val X = freshUVar()
    (ExprType(X), Map('this -> X), emptyCReqs, emptyCons)
  }
}
case object QThis extends Expr(simple(Seq(classOf[TypeName]))){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = ???
}

// Comparison Operators
case object Gt extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons :+ widen :+ t1inNum :+ t2inNum)
  }
}
case object GtEq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons :+ widen :+ t1inNum :+ t2inNum)
  }
}
case object Lt extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons :+ widen :+ t1inNum :+ t2inNum)
  }
}
case object LtEq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons :+ widen :+ t1inNum :+ t2inNum)
  }
}
case object Eq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val widen = PrimitiveWidening(t1, t2)
    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons :+ widen)
  }
}
case object NotEq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val widen = PrimitiveWidening(t1, t2)
    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons :+ widen)
  }
}

// Arithmetic Operators
case object Plus extends Expr(simple(cExpr, cExpr) orElse (simple(cExpr))){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = kids.size match {
    case 1 => {
      val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

      val X = freshUVar()
      val tInNum = OneOf(t, numTypes)
      val xInNumOps = OneOf(X, numericOpsTypes)
      val eq = Equality(t, X)

      (ExprType(X), vReqs, cReqs, cons :+ tInNum :+ xInNumOps :+ eq)
    }
    case 2 => {
      val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
      val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

      val X = freshUVar()

      val widenString = PrimitiveWideningString(t1, t2)
      val widenEq = PrimitiveWideningEq(X, t1, t2)
      val oneOfCons = Seq(OneOf(t1, TString +: numTypes)
                        , OneOf(t2, TString +: numTypes)
                        , OneOf( X, TString +: numericOpsTypes))

      val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
      val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widenString :+ widenEq

      (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
    }
  }
}
case object Minus extends Expr(simple(cExpr, cExpr) orElse (simple(cExpr))){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = kids.size match {
    case 1 => {
      val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

      val X = freshUVar()
      val tInNum = OneOf(t, numTypes)
      val xInNumOps = OneOf(X, numericOpsTypes)
      val eq = Equality(t, X)

      (ExprType(X), vReqs, cReqs, cons :+ tInNum :+ xInNumOps :+ eq)
    }
    case 2 => {
      val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
      val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

      val X = freshUVar()

      val widen = PrimitiveWidening(t1, t2)
      val widenEq = PrimitiveWideningEq(X, t1, t2)
      val oneOfCons = Seq(OneOf(t1, numTypes)
                        , OneOf(t2, numTypes)
                        , OneOf( X, numericOpsTypes))

      val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
      val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widen :+ widenEq

      (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
    }
  }
}
case object Mul extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, numTypes)
                      , OneOf(t2, numTypes)
                      , OneOf( X, numericOpsTypes))

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widen :+ widenEq

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object Div extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, numTypes)
                      , OneOf(t2, numTypes)
                      , OneOf( X, numericOpsTypes))

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widen :+ widenEq

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object Remain extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, numTypes)
                      , OneOf(t2, numTypes)
                      , OneOf( X, numericOpsTypes))

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widen :+ widenEq

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object PreIncr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object PostIncr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object PreDecr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object PostDecr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object LeftShift extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val t1InIntegral = OneOf(t1, integralTypes)
    val t2InIntegral = OneOf(t2, integralTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons :+ t1InIntegral :+ t2InIntegral

    (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object RightShift extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val t1InIntegral = OneOf(t1, integralTypes)
    val t2InIntegral = OneOf(t2, integralTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons :+ t1InIntegral :+ t2InIntegral

    (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object URightShift extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val t1InIntegral = OneOf(t1, integralTypes)
    val t2InIntegral = OneOf(t2, integralTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons :+ t1InIntegral :+ t2InIntegral

    (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}

// Bitwise Operators
case object Complement extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val tIsIntegral = OneOf(t, integralTypes)

    (ExprType(t), vReqs, cReqs, cons :+ tIsIntegral)
  }
}
case object And extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, TBoolean() +: integralTypes)
                      , OneOf(t2, TBoolean() +: integralTypes)
                      , OneOf( X, numericOpsTypes)) // TODO: + Boolean?

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widen :+ widenEq

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object Or extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, TBoolean() +: integralTypes)
                      , OneOf(t2, TBoolean() +: integralTypes)
                      , OneOf( X, numericOpsTypes)) // TODO: + Boolean?

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widen :+ widenEq

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object ExcOr extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, TBoolean() +: integralTypes)
                      , OneOf(t2, TBoolean() +: integralTypes)
                      , OneOf( X, numericOpsTypes)) // TODO: + Boolean?

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons ++ oneOfCons :+ widen :+ widenEq

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}

// Logical Operators
case object Not extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val tIsBool = Equality(TBoolean(), t)

    (ExprType(TBoolean()), vReqs, cReqs, cons :+ tIsBool)
  }
}
case object LazyAnd extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val t1IsBool = Equality(TBoolean(), t1)
    val t2IsBool = Equality(TBoolean(), t2)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons :+ t1IsBool :+ t2IsBool

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}
case object LazyOr extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ

    val t1IsBool = Equality(TBoolean(), t1)
    val t2IsBool = Equality(TBoolean(), t2)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)
    val cons = cons1 ++ cons2 ++ mCons :+ t1IsBool :+ t2IsBool

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2), cons)
  }
}

// Conditional Operator (Expr ? Expr : Expr)
case object Cond extends Expr(simple(cExpr, cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): StepResult = {
    val (ExprType(t1), vReqs1, cReqs1, cons1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2, cons2) = kids(1).typ
    val (ExprType(t3), vReqs3, cReqs3, cons3) = kids(2).typ

    val X = freshUVar()

    val t1IsBool = Equality(TBoolean(), t1)
    val widen = PrimitiveWidening(t2, t3)
    val widenEq = PrimitiveWideningEq(X, t2, t3)
    val t2isPrimitive = OneOf(t2, primTypes)
    val t3isPrimitive = OneOf(t3, primTypes)
    val XisNumOps = OneOf(X, TBoolean() +: numericOpsTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs2, vReqs3) // TODO: expand merge to multiple operands
    val cReqs = mergeCReqs(mergeCReqs(cReqs1, cReqs2), cReqs3) // TODO: expand merge to multiple operands
    val cons = cons1 ++ cons2 ++ cons3 ++ mCons :+ t1IsBool :+ widen :+ widenEq :+ t2isPrimitive :+ t3isPrimitive :+ XisNumOps

    (ExprType(X), mReqs, cReqs, cons)
  }
}

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