package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{Context, NodeKind, SyntaxChecking}
import incremental.java.syntax.expr.Expr._
/**
 * Created by qwert on 28.09.15.
 */

// Arithmetic Operators
case object Plus extends Expr(simple(cExpr, cExpr) orElse simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = kids.size match {
    case 1 => {
      val (ExprType(t), vReqs, cReqs) = kids(0).typ

      val X = freshUVar()
      val tInNum = OneOf(t, numTypes)
      val xInNumOps = OneOf(X, numericOpsTypes)
      val eq = Equality(t, X)

      context.addConstraints(tInNum, xInNumOps, eq)

      (ExprType(X), vReqs, cReqs)
    }
    case 2 => {
      val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
      val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

      val X = freshUVar()

      val widenString = PrimitiveWideningString(t1, t2)
      val widenEq = PrimitiveWideningEq(X, t1, t2)
      val oneOfCons = Seq(OneOf(t1, TString +: numTypes)
        , OneOf(t2, TString +: numTypes)
        , OneOf( X, TString +: numericOpsTypes))

      val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

      context.addConstraints(widenString, widenEq)
      context.addConstraintSeq(oneOfCons)
      context.addConstraintSeq(mCons)

      (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
    }
  }
}
case object Minus extends Expr(simple(cExpr, cExpr) orElse simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = kids.size match {
    case 1 => {
      val (ExprType(t), vReqs, cReqs) = kids(0).typ

      val X = freshUVar()
      val tInNum = OneOf(t, numTypes)
      val xInNumOps = OneOf(X, numericOpsTypes)
      val eq = Equality(t, X)

      context.addConstraints(tInNum, xInNumOps, eq)

      (ExprType(X), vReqs, cReqs)
    }
    case 2 => {
      val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
      val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

      val X = freshUVar()

      val widen = PrimitiveWidening(t1, t2)
      val widenEq = PrimitiveWideningEq(X, t1, t2)
      val oneOfCons = Seq(OneOf(t1, numTypes)
        , OneOf(t2, numTypes)
        , OneOf( X, numericOpsTypes))

      val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

      context.addConstraints(widen, widenEq)
      context.addConstraintSeq(oneOfCons)
      context.addConstraintSeq(mCons)

      (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
    }
  }
}
case object Mul extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, numTypes)
      , OneOf(t2, numTypes)
      , OneOf( X, numericOpsTypes))

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, widenEq)
    context.addConstraintSeq(oneOfCons)
    context.addConstraintSeq(mCons)

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object Div extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, numTypes)
      , OneOf(t2, numTypes)
      , OneOf( X, numericOpsTypes))

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, widenEq)
    context.addConstraintSeq(oneOfCons)
    context.addConstraintSeq(mCons)

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object Remain extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, numTypes)
      , OneOf(t2, numTypes)
      , OneOf( X, numericOpsTypes))

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, widenEq)
    context.addConstraintSeq(oneOfCons)
    context.addConstraintSeq(mCons)

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object PreIncr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t), vReqs, cReqs) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    context.addConstraints(eq, tInNum, xInNumOps)

    (ExprType(X), vReqs, cReqs)
  }
}
case object PostIncr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t), vReqs, cReqs) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    context.addConstraints(eq, tInNum, xInNumOps)

    (ExprType(X), vReqs, cReqs)
  }
}
case object PreDecr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t), vReqs, cReqs) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    context.addConstraints(eq, tInNum, xInNumOps)

    (ExprType(X), vReqs, cReqs)
  }
}
case object PostDecr extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t), vReqs, cReqs) = kids(0).typ

    val X = freshUVar()

    val eq = Equality(t, X)
    val tInNum = OneOf(t, numTypes)
    val xInNumOps = OneOf(X, numericOpsTypes)

    context.addConstraints(eq, tInNum, xInNumOps)

    (ExprType(X), vReqs, cReqs)
  }
}
case object LeftShift extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val t1InIntegral = OneOf(t1, integralTypes)
    val t2InIntegral = OneOf(t2, integralTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(t1InIntegral, t2InIntegral)
    context.addConstraintSeq(mCons)

    (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object RightShift extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val t1InIntegral = OneOf(t1, integralTypes)
    val t2InIntegral = OneOf(t2, integralTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(t1InIntegral, t2InIntegral)
    context.addConstraintSeq(mCons)

    (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object URightShift extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val t1InIntegral = OneOf(t1, integralTypes)
    val t2InIntegral = OneOf(t2, integralTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(t1InIntegral, t2InIntegral)
    context.addConstraintSeq(mCons)

    (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}

// Bitwise Operators
case object Complement extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t), vReqs, cReqs) = kids(0).typ

    val tIsIntegral = OneOf(t, integralTypes)

    context.addConstraint(tIsIntegral)

    (ExprType(t), vReqs, cReqs)
  }
}
case object And extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, TBoolean() +: integralTypes)
      , OneOf(t2, TBoolean() +: integralTypes)
      , OneOf( X, numericOpsTypes)) // TODO: + Boolean?

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, widenEq)
    context.addConstraintSeq(oneOfCons)
    context.addConstraintSeq(mCons)

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object Or extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, TBoolean() +: integralTypes)
      , OneOf(t2, TBoolean() +: integralTypes)
      , OneOf( X, numericOpsTypes)) // TODO: + Boolean?

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, widenEq)
    context.addConstraintSeq(oneOfCons)
    context.addConstraintSeq(mCons)

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object ExcOr extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val X = freshUVar()

    val widen = PrimitiveWidening(t1, t2)
    val widenEq = PrimitiveWideningEq(X, t1, t2)
    val oneOfCons = Seq(OneOf(t1, TBoolean() +: integralTypes)
      , OneOf(t2, TBoolean() +: integralTypes)
      , OneOf( X, numericOpsTypes)) // TODO: + Boolean?

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, widenEq)
    context.addConstraintSeq(oneOfCons)
    context.addConstraintSeq(mCons)

    (ExprType(X), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}