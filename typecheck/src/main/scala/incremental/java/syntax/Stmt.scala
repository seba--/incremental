package incremental.java.syntax

import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax.expr.Expr
import incremental.{Context, NodeKind, SyntaxChecking}
import incremental.java.syntax.JavaSyntaxChecker._
import incremental.java.JavaCheck._

/**
 * Created by qwert on 05.04.15.
 */


abstract class Stm(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[Constraint, Result](syntaxcheck) with NT_BlockStm
object Stm {
  val cStm = classOf[Stm]
}
import Stm._
import Expr._

case object Empty extends Stm(simple()) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = (StmOk, emptyVReqs, emptyCReqs)
}
case object Labeled extends Stm(simple(Seq(classOf[String]), cStm)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object ExprStm extends Stm(simple(cExpr)) { // TODO: restrict possible expressions in syntax
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (exprType, vReqs, cReqs) = kids(0).typ
    (StmOk, vReqs, cReqs)
  }
}

case object If extends Stm(simple(cExpr, cStm) orElse simple(cExpr, cStm, cStm)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = kids.size match {
    case 2 => // If
      val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
      val (StmOk, vReqs2, cReqs2) = kids(1).typ

      val t1IsBool = Equality(t1, TBoolean())
      val (mcons, mreqs) = mergeVReqs(vReqs1, vReqs2)

      context.addConstraint(t1IsBool)
      context.addConstraintSeq(mcons)

      (StmOk, mreqs, mergeCReqs(cReqs1, cReqs2))
    case 3 => // IfElse
      val (ExprType(t), vReqs1, cReqs1) = kids(0).typ
      val (StmOk, vReqs2, cReqs2) = kids(1).typ
      val (StmOk, vReqs3, cReqs3) = kids(2).typ

      val tIsBool = Equality(t, TBoolean())
      val (mcons, mvreqs) = mergeVReqs(Seq(vReqs1, vReqs2, vReqs3))

      context.addConstraint(tIsBool)
      context.addConstraintSeq(mcons)

      val mcreqs = mergeCReqs(mergeCReqs(cReqs1, cReqs2), cReqs3)

      (StmOk, mvreqs, mcreqs)
  }
}

case object AssertStm extends Stm(simple(cExpr) orElse simple(cExpr, cExpr)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

// Switch
trait NT_SwitchBlock
trait NT_SwitchGroup
trait NT_SwitchLabel

case object Switch extends Stm(simple(cExpr, SwitchBlock.getClass)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object SwitchBlock extends NodeKind[Constraint, Result](noLits andAlso unsafeAllKids(classOf[NT_SwitchGroup], classOf[NT_SwitchLabel])) with NT_SwitchBlock  {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object SwitchGroup extends NodeKind[Constraint, Result](noLits andAlso nonEmptyKids andAlso unsafeAllKids(classOf[NT_SwitchLabel], classOf[NT_BlockStm])) with NT_SwitchGroup {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object Case extends NodeKind[Constraint, Result](simple(cExpr)) with NT_SwitchLabel {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object Default extends NodeKind[Constraint, Result](simple()) with NT_SwitchLabel {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

// Loops
case object While extends Stm(simple(cExpr, cStm)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (ExprType(t1), vReqs1, cReqs1) = kids(0).typ
    val (StmOk, vReqs2, cReqs2) = kids(1).typ

    val exprIsBool = Equality(t1, TBoolean()) // TODO: change Equality with OneOf for boxing/unboxing?
    val (mcons, mreqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraint(exprIsBool)
    context.addConstraintSeq(mcons)

    (StmOk, mreqs, mergeCReqs(cReqs1, cReqs2))
  }
}
case object DoWhile extends Stm(simple(cStm, cExpr)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (StmOk, vReqs1, cReqs1) = kids(0).typ
    val (ExprType(t2), vReqs2, cReqs2) = kids(1).typ

    val exprIsBool = Equality(t2, TBoolean()) // TODO: change Equality with OneOf for boxing/unboxing?
    val (mcons, mreqs) = mergeVReqs(vReqs1, vReqs2)

    context.addConstraint(exprIsBool)
    context.addConstraintSeq(mcons)

    (StmOk, mreqs, mergeCReqs(cReqs1, cReqs2))
  }
}

case object For extends Stm((noLits andAlso uFollowedByKids(Seq(classOf[NT_LocalVarDec], cStm), cExpr)) orElse // first the statement then the increasing expressions
                            (noLits andAlso uFollowedByKids(Seq(classOf[NT_LocalVarDec], cExpr, cStm), cExpr))) {// TODO: orElse: "for" "(" {Expr ","}* ";" Expr? ";" {Expr ","}* ")" Stm -> Stm {cons("For")}
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object ForEach extends Stm(simple(classOf[FormalParam], cExpr, cStm)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

case object Break extends Stm(simple() orElse simple(Seq(classOf[String]))) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object Continue extends Stm(simple() orElse simple(Seq(classOf[String]))) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object Return extends Stm(simple() orElse simple(cExpr)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object Throw extends Stm(simple(cExpr)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

case object Synchronized extends Stm(simple(cExpr, Block.getClass)) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

// CatchClause
trait NT_CatchClause

case object Try extends Stm((noLits andAlso nonEmptyKids andAlso uFollowedByKids(classOf[NT_Block], classOf[NT_CatchClause])) orElse
                            (noLits andAlso uFollowedByKids(Seq(classOf[NT_Block], classOf[NT_Block]), classOf[NT_CatchClause]))) {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object Catch extends NodeKind[Constraint, Result](simple(classOf[FormalParam], Block.getClass)) with NT_CatchClause {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

// Blocks
trait NT_BlockStm
trait NT_Block extends NT_MethodBody

case object Block extends Stm(noLits andAlso unsafeAllKids(classOf[NT_BlockStm])) with NT_Block {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object ClassDecStm extends NodeKind[Constraint, Result](noLits andAlso unsafeKids(Seq())) with NT_BlockStm { // TODO: classOf[NT_ClassDec] in kids
def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}

// LocalVariableDeclarations
trait NT_LocalVarDecStm extends NT_BlockStm
trait NT_LocalVarDec

case object LocalVarDecStm extends NodeKind[Constraint, Result](simple(LocalVarDec.getClass)) with NT_LocalVarDecStm  {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}
case object LocalVarDec extends NodeKind[Constraint, Result](litsFollowedBy(classOf[VarMod], classOf[Type]) andAlso unsafeAllKids(classOf[NT_Anno], classOf[NT_VarDec]) andAlso nonEmptyKids) with NT_LocalVarDec {
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ???
}