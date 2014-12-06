package incremental.pcf.with_subtyping

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import TypeOps._
import Type.Companion._
import incremental.pcf._
import incremental.{TypeCheckerFactory, Exp_, TypeChecker, Util}

/**
 * Created by oliver on 27.11.14.
 */
class DownUpChecker extends TypeChecker[Type] {

  val constraint = new Constr
  import constraint._
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime
  def mergeSolutionTime = constraint.mergeSolutionTime

  val preparationTime = 0.0
  var typecheckTime = 0.0

  type Result = (Type, CSet)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
      try {
        val (t_, sol_) = typecheck(root, Map())
        val (sigma, notyet, unsat) = sol_.tryFinalize.state
        val t = t_.subst(sigma)
        if (!(notyet.isEmpty && unsat.isEmpty))
          Right(s"Unresolved constraints notyet: $notyet\nunsat: ${unsat}, type $t")
        else
          Left(t)
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      }
    )
    typecheckTime += ctime
    res
  }

  def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Num =>
      (TNum, CSet())
    case k if k == Add || k == Mul =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val sol = sol1 <-- sol2 <-- Equal(TNum, t1) <-- Equal(TNum, t2)
      (TNum, sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, CSet())
      }
    case App =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val X = freshTVar(false)
      val Y = freshTVar()
      val sol = sol1 <-- sol2 + Equal(X -->: Y, t1) <-- Subtype(t2, X)
      (Y.subst(sol.solution), sol)
    case Abs if e.lits(0).isInstanceOf[Symbol] =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshTVar()
      val (t, subsol) = typecheck(e.kids(0), ctx + (x -> annotatedT))
      (TFun(annotatedT, t), subsol)
    case If0 =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val (t3, sol3) = typecheck(e.kids(2), ctx)
      val Xjoin = freshTVar()
      val sol = sol1 <-- sol2 <-- sol3 + Equal(t1, TNum) + Subtype(t2, Xjoin) <-- Subtype(t3, Xjoin)
      (Xjoin.subst(sol.solution), sol)
    case Fix =>
      val (t, subsol) = typecheck(e.kids(0), ctx)
      val X = freshTVar(false)
      val Y = freshTVar()
      val sol = subsol + Equal(t, X -->: Y) <-- Subtype(Y, X)
      (X.subst(sol.solution), sol)
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new DownUpChecker
}
