package incremental.java.syntax.expr

import constraints.javacons._
import incremental.Context
import incremental.Node._
import incremental.SyntaxChecking._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.java.syntax.expr.Expr._

/**
 * Created by qwert on 29.09.15.
 */

// Assignment Operators
case object Assign extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = kids(0).kind match {
    case x : ExprName.type =>
      val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
      val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

      val subtypewiden = SubtypeOrDirectedWidening(t2, t1)
      val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

      context.addConstraintSeq(mCons)
      context.addConstraint(subtypewiden)

      (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2))
    case f : FieldAccess =>
      // merge into just one case?
      val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
      val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

      val subtype = Subtype(t2, t1)
      val (mCons, mReqs) = mergeVReqs(vReqs1, vReqs2)

      context.addConstraintSeq(mCons)
      context.addConstraint(subtype)

      (ExprType(t1), mReqs, mergeCReqs(cReqs1, cReqs2))
    case arr : ArrayAccess.type =>
      ???
  }
}
case object AssignMul extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignDiv extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignRemain extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignPlus extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignMinus extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignLeftShift extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignRightShift extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignURightShift extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignAnd extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignExcOr extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object AssignOr extends Expr(simple(ExprName.getClass, cExpr) orElse simple(classOf[FieldAccess], cExpr) orElse simple(ArrayAccess.getClass, cExpr)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}