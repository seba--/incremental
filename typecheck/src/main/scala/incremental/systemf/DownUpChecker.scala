package incremental.systemf
import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type.Companion._
import incremental._

/**
 * Created by seba on 14/11/14.
 */
class DownUpChecker extends TypeChecker[Type] {

  val constraint = new ConstraintOps
  import constraint._

  val preparationTime = 0.0
  var typecheckTime = 0.0
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime
  def mergeSolutionTime = constraint.mergeSolutionTime

  type Result = (Type, Solution)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
      try {
        val (t, sol_) = typecheck(root, Map(), Set())
        val sol = sol_.tryFinalize
        if (sol.isSolved)
          Left(t.subst(sol.substitution))
        else
          Right(s"Unresolved constraints ${sol.unsolved}, type ${t.subst(sol.substitution)}, subst ${sol.substitution}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
        case ex: UnboundTVariable => Right(s"Unbound type variable ${ex.x} in context ${ex.ctx}")
      }
    )
    typecheckTime += ctime
    res
  }

  def typecheck(e: Exp_[Result], ctx: TSubst, tctx: Set[Symbol]): Result = e.kind match {
    case Num => (TNum, emptySol)
    case k if k == Add || k == Mul =>
      val (t1, sol1) = typecheck(e.kids(0), ctx, tctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx, tctx)
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
      val (t1, sol1) = typecheck(e.kids(0), ctx, tctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx, tctx)
      val subsol = sol1 ++ sol2

      val X = freshTVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val sol = solve(fcons, subsol)

      (X.subst(sol.substitution), sol)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X =
        if (e.lits.size == 2) {
          val t = e.lits(1).asInstanceOf[Type]
          for (x <- t.freeTVars)
            if (!tctx.contains(x))
              throw UnboundTVariable(x, tctx)
          t
        }
        else
          freshTVar()


      val (t, subsol) = typecheck(e.kids(0), ctx + (x -> X), tctx)
      (TFun(X.subst(subsol.substitution), t), subsol)
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshTVar())

      val (t, subsol) = typecheck(e.kids(0), ctx ++ (xs zip Xs), tctx)

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X.subst(subsol.substitution), tfun)
      }

      (tfun, subsol)
    case If0 =>
      val (t1, sol1) = typecheck(e.kids(0), ctx, tctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx, tctx)
      val (t3, sol3) = typecheck(e.kids(2), ctx, tctx)
      val subsol = sol1 ++ sol2 ++ sol3

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)
      val sol = solve(Seq(cond, body), subsol)

      (t2.subst(sol.substitution), sol)
    case Fix =>
      val (t, subsol) = typecheck(e.kids(0), ctx, tctx)

      val X = freshTVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = solve(fixCons, subsol)

      (X.subst(sol.substitution), sol)

    case TAbs if (e.lits(0).isInstanceOf[Symbol]) =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val (t, subsol) = typecheck(e.kids(0), ctx, tctx + alpha)
      (TUniv(alpha, t), subsol)

    case TApp =>
      val (t1, subsol) = typecheck(e.kids(0), ctx, tctx)
      val t = e.lits(0).asInstanceOf[Type]

      for (x <- t.freeTVars)
        if (!tctx.contains(x))
          throw UnboundTVariable(x, tctx)

      val Xalpha = freshTVar().x
      val Xbody = freshTVar()
      val Xres = freshTVar()

      val ucons = EqConstraint(UUniv(Xalpha, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      val sol = solve(Seq(ucons, vcons), subsol)

      (Xres.subst(sol.substitution), sol)
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException
case class UnboundTVariable(x: Symbol, ctx: Set[Symbol]) extends RuntimeException

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new DownUpChecker
}