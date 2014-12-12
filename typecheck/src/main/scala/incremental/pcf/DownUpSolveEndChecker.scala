package incremental.pcf

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type.Companion._
import incremental._

/**
 * Created by seba on 14/11/14.
 */
class DownUpSolveEndChecker extends TypeChecker[Type] {

  val constraint = new ConstraintOps
  import constraint._

  val preparationTime = 0.0
  var typecheckTime = 0.0
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime
  def mergeSolutionTime = constraint.mergeSolutionTime

  type Result = (Type, Seq[Constraint])

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
      try {
        val (t, cons) = typecheck(root, Map())
        val sol = solve(cons)
        if (sol.isSolved)
          Left(t.subst(sol.substitution))
        else
          Right(s"Unresolved constraints ${sol.unsolved}, type ${t.subst(sol.substitution)}, subst ${sol.substitution}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      }
    )
    typecheckTime += ctime
    res
  }

  def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Num => (TNum, Seq())
    case k if k == Add || k == Mul =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      (TNum, lcons +: rcons +: (sol1 ++ sol2))
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, Seq())
      }
    case App =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)

      val X = freshTVar()
      val fcons = EqConstraint(TFun(t2, X), t1)

      (X, fcons +: (sol1 ++ sol2))
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X =
        if (e.lits.size == 2)
          e.lits(1).asInstanceOf[Type]
        else
          freshTVar()

      val (t, subsol) = typecheck(e.kids(0), ctx + (x -> X))
      (TFun(X, t), subsol)
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshTVar())

      val (t, subsol) = typecheck(e.kids(0), ctx ++ (xs zip Xs))

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X, tfun)
      }

      (tfun, subsol)
    case If0 =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val (t3, sol3) = typecheck(e.kids(2), ctx)
      val subsol = sol1 ++ sol2 ++ sol3

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      (t2, cond +: body +: subsol)
    case Fix =>
      val (t, subsol) = typecheck(e.kids(0), ctx)

      val X = freshTVar()
      val fixCons = EqConstraint(t, TFun(X, X))

      (X, fixCons +: subsol)
  }
}

object DownUpSolveEndCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new DownUpSolveEndChecker
}