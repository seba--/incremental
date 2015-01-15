package incremental.pcf

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type.Companion._
import incremental._

/**
 * Created by seba on 14/11/14.
 */
class DownUpChecker extends DUChecker[Type] {
  type CSystem = ConstraintOps.type
  val cs = ConstraintOps
  import cs._
  import localState.gen._

  def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Num => (TNum, emptyCSet)
    case k if k == Add || k == Mul =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val subsol = sol1 ++ sol2

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      val sol = subsol ++ Seq(lcons, rcons)

      (TNum, sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, emptyCSet)
      }
    case App =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val subsol = sol1 ++ sol2

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val sol = subsol + fcons

      (X.subst(sol.substitution), sol)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X =
        if (e.lits.size == 2)
          e.lits(1).asInstanceOf[Type]
        else
          freshUVar()

      val (t, subsol) = typecheck(e.kids(0), ctx + (x -> X))
      (TFun(X.subst(subsol.substitution), t), subsol)
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshUVar())

      val (t, subsol) = typecheck(e.kids(0), ctx ++ (xs zip Xs))

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X.subst(subsol.substitution), tfun)
      }

      (tfun, subsol)
    case If0 =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val (t3, sol3) = typecheck(e.kids(2), ctx)
      val subsol = sol1 ++ sol2 ++ sol3

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)
      val sol = subsol ++ Seq(cond, body)

      (t2.subst(sol.substitution), sol)
    case Fix =>
      val (t, subsol) = typecheck(e.kids(0), ctx)

      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = subsol + fixCons

      (X.subst(sol.substitution), sol)
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new DownUpChecker
}