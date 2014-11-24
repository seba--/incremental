package incremental.systemf

import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type._
import incremental._

/**
 * Created by seba on 14/11/14.
 */
class DownUpChecker extends TypeChecker {

  val constraint = new Constraint
  import constraint._

  val preparationTime = 0.0
  var typecheckTime = 0.0
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime

  type Result = (Type, TSubst, Unsolvable)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
      try {
        val (t, s, unres) = typecheck(root, Map())
        if (unres.isEmpty)
          Left(t.subst(s))
        else
          Right(s"Unresolved constraints $unres, type ${t.subst(s)}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      })
    typecheckTime += ctime
    res
  }

  def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Num => (TNum, Map(), Seq())
    case k if k == Add || k == Mul =>
      val (t1, s1, unres1) = typecheck(e.kids(0), ctx)
      val (t2, s2, unres2) = typecheck(e.kids(1), ctx)
      val subsol = mergeSolution((s1, unres1), (s2, unres2))

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      val (s, unres) = solve(Seq(lcons, rcons), subsol)

      (TNum, s, unres)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, Map(), Seq())
      }
    case App =>
      val (t1, s1, unres1) = typecheck(e.kids(0), ctx)
      val (t2, s2, unres2) = typecheck(e.kids(1), ctx)
      val subsol = mergeSolution((s1, unres1), (s2, unres2))

      val X = freshTVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (s, unres) = solve(fcons, subsol)

      (X.subst(s), s, unres)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X =
        if (e.lits.size == 2)
          e.lits(1).asInstanceOf[Type]
        else
          freshTVar()

      val (t, s, unres) = typecheck(e.kids(0), ctx + (x -> X))
      (TFun(X.subst(s), t), s, unres)
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshTVar())

      val (t, s, unres) = typecheck(e.kids(0), ctx ++ (xs zip Xs))

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X.subst(s), tfun)
      }

      (tfun, s, unres)
    case If0 =>
      val (t1, s1, unres1) = typecheck(e.kids(0), ctx)
      val (t2, s2, unres2) = typecheck(e.kids(1), ctx)
      val (t3, s3, unres3) = typecheck(e.kids(2), ctx)
      val subsol = mergeSolution((s1, unres1), mergeSolution((s2, unres2), (s3, unres3)))

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)
      val (s, unres) = solve(Seq(cond, body), subsol)

      (t2.subst(s), s, unres)
    case Fix =>
      val (t, s1, unres1) = typecheck(e.kids(0), ctx)

      val X = freshTVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val (s, unres) = solve(fixCons, (s1, unres1))

      (X.subst(s), s, unres)
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException

object DownUpCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new DownUpChecker
}