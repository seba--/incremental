package incremental.pcf.with_subtyping

import incremental.Exp.Exp
import TypeOps._
import Type.Companion._
import incremental.pcf._
import incremental._

/**
 * Created by oliver on 27.11.14.
 */
class DownUpChecker extends DUChecker[Type] {
  type CSystem = ConstraintOps.type
  val cs = ConstraintOps
  import cs._
  import localState.gen._

  def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Num =>
      (TNum, emptyCSet)
    case k if k == Add || k == Mul =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val sol = (sol1 ++ sol2 + Equal(TNum, t1) + Equal(TNum, t2)).trySolve
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
      val X = freshUVar(false)
      val Y = freshUVar()
      val sol = (sol1 ++ sol2 + Equal(X -->: Y, t1) + Subtype(t2, X)).trySolve
      (Y.subst(sol.substitution), sol)
    case Abs if e.lits(0).isInstanceOf[Symbol] =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
      val (t, subsol) = typecheck(e.kids(0), ctx + (x -> annotatedT))
      (TFun(annotatedT, t), subsol)
    case If0 =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val (t3, sol3) = typecheck(e.kids(2), ctx)
      val Xjoin = freshUVar()
      val sol = (sol1 ++ sol2 ++ sol3 + Equal(t1, TNum) + Join(Xjoin, Set(t2, t3))).trySolve
      (Xjoin.subst(sol.substitution), sol)
    case Fix =>
      val (t, subsol) = typecheck(e.kids(0), ctx)
      val X = freshUVar(false)
      val Y = freshUVar()
      val sol = (subsol + Equal(t, X -->: Y) + Subtype(Y, X)).trySolve
      (X.subst(sol.substitution), sol)
  }
}

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new DownUpChecker
}
