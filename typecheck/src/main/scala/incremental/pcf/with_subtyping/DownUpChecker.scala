package incremental.pcf.with_subtyping

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

  val solver = new Constraints
  import solver._
  def constraintCount = solver.constraintCount
  def mergeReqsTime = solver.mergeReqsTime
  def constraintSolveTime = solver.constraintSolveTime


  val preparationTime = 0.0
  var typecheckTime = 0.0

  type Result = (Type, CSet)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
      try {
        val (t, unres) = typecheck(root, Map())
        Left(t.subst(unres.finalized))
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
        case ConstraintException(msg) => Right(s"There were unresolved constraints: $msg")
      })
    typecheckTime += ctime
    res
  }

  def typecheck(e: Exp_[Result], ctx: TSubst): Result = e.kind match {
    case Num => (TNum, empty)
    case k if k == Add || k == Mul =>
      val (t1, unres1) = typecheck(e.kids(0), ctx)
      val (t2, unres2) = typecheck(e.kids(1), ctx)
      val lcons = normalizeEq(t1, TNum)
      val rcons = normalizeEq(t2, TNum)
      (TNum, unres1 && unres2 && lcons && rcons)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, empty)
      }
    case App =>
      val (t1, unres1) = typecheck(e.kids(0), ctx)
      val (t2, unres2) = typecheck(e.kids(1), ctx)
      val (x,y) = (freshTVar(), freshTVar())
      val fcons = normalizeEq(t1, x -->: y)
      val argcons = normalizeSub(Bot, t2, x)
      (y, unres1 && unres2 && fcons && argcons)
    case Abs if (e.lits(0).isInstanceOf[Symbol] && e.lits(1).isInstanceOf[Type]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = e.lits(1).asInstanceOf[Type]
      val (t, unres) = typecheck(e.kids(0), ctx + (x -> annotatedT))
      (annotatedT -->: t, unres)
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
      val (t1, cons1) = typecheck(e.kids(0), ctx)
      val (t2, cons2) = typecheck(e.kids(1), ctx)
      val (t3, cons3) = typecheck(e.kids(2), ctx)
      val t4 = freshTVar()
      val ccons = normalizeEq(t1, TNum)
      val tcons = normalizeSub(Bot, t2, t4)
      val econs = normalizeSub(Bot, t3, t4)
      val newcons = cons1 && cons2 && cons3 && ccons && tcons && econs
      (t4, newcons)
    case Fix =>
      val (t, cons) = typecheck(e.kids(0), ctx)
      val X = freshTVar()
      val fixCons = normalizeEq(t, X -->: X)
      val newcons = cons && fixCons
      (X, newcons)
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException

object DownUpCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new DownUpChecker
}
