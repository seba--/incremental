package incremental.pcf.with_subtyping

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type._
import incremental._
import incremental.pcf._
import TypeOps._

/**
 * Created by oliver on 27.11.14.
 */
class DownUpChecker extends TypeChecker {

  val constraint = new ConstraintOps
  import constraint._
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime

  val preparationTime = 0.0
  var typecheckTime = 0.0

  type Result = (Type, Solution)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
      try {
        val (t, sol_) = typecheck(root, Map())
        val sol = sol_.trySolveNow
        if (sol.isSolved)
          Left(t.subst(sol.solution))
        else
          Right(s"Unresolved constraints ${sol.unsolved}, type ${t.subst(sol.solution)}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      }
    )
    typecheckTime += ctime
    res
  }

  def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Num => (TNum, emptySol)
    case k if k == Add || k == Mul =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val subsol = sol1 ++ sol2

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      val sol = solve(Seq(lcons, rcons), subsol)

      (TNum, sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, emptySol)
      }
    case App =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val subsol = sol1 ++ sol2

      val X = freshTVar()
      val Y = freshTVar()
      val fcons = EqConstraint(TFun(X, Y), t1)
      val acons = SubConstraint(t2, X)

      val sol = solve(Seq(fcons, acons), subsol)
      (Y.subst(sol.solution), sol)
    case Abs if (e.lits(0).isInstanceOf[Symbol] && e.lits(1).isInstanceOf[Type]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = e.lits(1).asInstanceOf[Type]
      val (t, subsol) = typecheck(e.kids(0), ctx + (x -> annotatedT))
      (TFun(annotatedT, t), subsol)
    /*case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshTVar())

      val (t, s, unres) = typecheck(e.kids(0), ctx ++ (xs zip Xs))

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X.subst(s), tfun)
      }

      (tfun, s, unres)*/
    case If0 =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val (t3, sol3) = typecheck(e.kids(2), ctx)
      val subsol = sol1 ++ sol2 ++ sol3

      val Xjoin = freshTVar()
      val ccons = EqConstraint(t1, TNum)
      val bodycons = EqJoinConstraint(t2, t3, Xjoin)

      val sol = solve(Seq(ccons, bodycons), subsol)
      (Xjoin.subst(sol.solution), sol)
    case Fix =>
      val (t, subsol) = typecheck(e.kids(0), ctx)
      val X = freshTVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = solve(fixCons, subsol)
      (X.subst(sol.solution), sol)
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException

object DownUpCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new DownUpChecker
}
