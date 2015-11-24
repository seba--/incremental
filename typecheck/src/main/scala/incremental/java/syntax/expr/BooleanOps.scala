package incremental.java.syntax.expr

/**
 * Created by qwert on 28.09.15.
 */

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{Context, NodeKind, SyntaxChecking}
import incremental.java.syntax.expr.Expr._

// Comparison Operators
case object Gt extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, t1inNum, t2inNum)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object GtEq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, t1inNum, t2inNum)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object Lt extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, t1inNum, t2inNum)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object LtEq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val widen: Constraint = PrimitiveWidening(t1, t2)
    val t1inNum: Constraint = OneOf(t1, numTypes)
    val t2inNum: Constraint = OneOf(t2, numTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(widen, t1inNum, t2inNum)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object Eq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val widen = PrimitiveWidening(t1, t2)
    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraint(widen)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object NotEq extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val widen = PrimitiveWidening(t1, t2)
    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraint(widen)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}

// Logical Operators
case object Not extends Expr(simple(cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t), vReqs, cReqs) = kids(0).typ

    val tIsBool = Equality(TBoolean(), t)

    context.addConstraint(tIsBool)

    (ExprType(TBoolean()), vReqs, cReqs)
  }
}
case object LazyAnd extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val t1IsBool = Equality(TBoolean(), t1)
    val t2IsBool = Equality(TBoolean(), t2)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(t1IsBool, t2IsBool)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object LazyOr extends Expr(simple(cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val t1IsBool = Equality(TBoolean(), t1)
    val t2IsBool = Equality(TBoolean(), t2)

    val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraints(t1IsBool, t2IsBool)
    context.addConstraintSeq(mCons)

    (ExprType(TBoolean()), mReqs, mergeCReqs(cReqs1, cReqs2))
  }
}

// Conditional Operator (Expr ? Expr : Expr)
case object Cond extends Expr(simple(cExpr, cExpr, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ
    val (ExprType(t3), vReqs3, cReqs3) = kids(2).typ

    val X = freshUVar()

    val t1IsBool = Equality(TBoolean(), t1)
    val widen = PrimitiveWidening(t2, t3)
    val widenEq = PrimitiveWideningEq(X, t2, t3)
    val t2isPrimitive = OneOf(t2, primTypes)
    val t3isPrimitive = OneOf(t3, primTypes)
    val XisNumOps = OneOf(X, TBoolean() +: numericOpsTypes)

    val (mCons, mReqs) = mergeVReqs(vReqs2, vReqs3) // TODO: expand merge to multiple operands
    val cReqs = mergeCReqs(mergeCReqs(cReqs1, cReqs2), cReqs3) // TODO: expand merge to multiple operands

    context.addConstraints(t1IsBool, widen, widenEq, t2isPrimitive, t3isPrimitive, XisNumOps)
    context.addConstraintSeq(mCons)

    (ExprType(X), mReqs, cReqs)
  }
}