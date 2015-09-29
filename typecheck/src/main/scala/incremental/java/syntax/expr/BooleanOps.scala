package incremental.java.syntax.expr

/**
 * Created by qwert on 28.09.15.
 */

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.{NodeKind, SyntaxChecking}
import incremental.java.syntax.expr.Expr._

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