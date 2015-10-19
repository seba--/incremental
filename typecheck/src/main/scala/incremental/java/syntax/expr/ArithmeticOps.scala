package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{NodeKind, SyntaxChecking}
import incremental.java.syntax.expr.Expr._
/**
 * Created by qwert on 28.09.15.
 */

// Arithmetic Operators
case object Plus extends Expr(simple(cExpr, cExpr) orElse (simple(cExpr))){
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = kids.size match {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = kids.size match {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object PostIncr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object PreDecr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object PostDecr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    (ExprType(X), vReqs, cReqs, cons :+ eq :+ tInNum :+ xInNumOps)
  }
}
case object LeftShift extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
    val (ExprType(t), vReqs, cReqs, cons) = kids(0).typ

    val tIsIntegral = OneOf(t, integralTypes)

    (ExprType(t), vReqs, cReqs, cons :+ tIsIntegral)
  }
}
case object And extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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
  def check(lits: Seq[Any], kids: Seq[Kid]): Result = {
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