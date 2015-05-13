package incremental.systemfomega

import constraints.StatKeys._
import constraints.normequality._
import incremental.{Util, Node_}
import incremental.Node._


/**
 * Created by seba on 14/11/14.
 */
abstract class DUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String
  
  type Ctx = Map[Symbol, Type]
  type TCtx = Map[Symbol, Kind]

  type StepResult[T] = (T, Seq[Constraint], Seq[CS])

  type Result[T] = (T, CS)

  def typecheckImpl(e: Node): Either[Type, TError] =
    localState.stats(TypeCheck) {
      try {
        val (t, sol_) = typecheckExpRec(e, Map(), Map())
        val sol = sol_.tryFinalize
        if (sol.isSolved)
          Left(t.subst(sol.substitution))
        else
          Right(s"Unresolved constraints ${sol.unsolved}, type ${t.subst(sol.substitution)}, subst ${sol.substitution}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
        case ex: UnboundTVariable => Right(s"Unbound type variable ${ex.x} in context ${ex.ctx}")
      }
    }

  def typecheckExpRec(e: Node, ctx: Ctx, tctx: TCtx): Result[Type] = {
    val (t, cons, css) = typecheckExpStep(e, ctx, tctx)
    val subcs = css.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res)
    val cs = subcs addNewConstraints cons
    (cs applyPartialSolution t, cs.propagate)
  }

  def typecheckTypeRec(e: Node, tctx: TCtx): Result[Kind] = {
    val (k, cons, css) = typecheckTypeStep(e, tctx)
    val subcs = css.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res)
    val cs = subcs addNewConstraints cons
    (cs applyPartialSolution k, cs.propagate)
  }

  def typecheckExpStep(e: Node, ctx: Ctx, tctx: TCtx): StepResult[Type] = e.kind match {
    case Num => (TNum(), Seq(), Seq())
    case k if k == Add || k == Mul =>
      val (t1, cs1) = typecheckExpRec(e.kids(0), ctx, tctx)
      val (t2, cs2) = typecheckExpRec(e.kids(1), ctx, tctx)
      val lcons = EqConstraint(TNum(), t1)
      val rcons = EqConstraint(TNum(), t2)
      (TNum(), Seq(lcons, rcons), Seq(cs1, cs2))

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, Seq(), Seq())
      }

    case App =>
      val (t1, cs1) = typecheckExpRec(e.kids(0), ctx, tctx)
      val (t2, cs2) = typecheckExpRec(e.kids(1), ctx, tctx)
      val X = freshUVar()
      (X, Seq(EqConstraint(TFun(t2, X), t1)), Seq(cs1, cs2))

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      
      if (e.kids(0).kind.isInstanceOf[Type.Kind]) {
        val (k, tcs) = typecheckTypeRec(e.kids(0), tctx)
        val X = Type.from(e.kids(0))
        val (t, cs) = typecheckExpRec(e.kids(1), ctx + (x -> X), tctx)
        (TFun(X, t), Seq(EqKindConstraint(k, KStar)), Seq(tcs, cs))
      }
      else {
        val X = freshUVar()
        val (t, cs) = typecheckExpRec(e.kids(0), ctx + (x -> X), tctx)
        (TFun(X, t), Seq(), Seq(cs))
      }


    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshUVar())
      val (t, cs) = typecheckExpRec(e.kids(0), ctx ++ (xs zip Xs), tctx)

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X, tfun)
      }

      (tfun, Seq(), Seq(cs))

    case If0 =>
      val (t1, cs1) = typecheckExpRec(e.kids(0), ctx, tctx)
      val (t2, cs2) = typecheckExpRec(e.kids(1), ctx, tctx)
      val (t3, cs3) = typecheckExpRec(e.kids(2), ctx, tctx)

      val cond = EqConstraint(TNum(), t1)
      val body = EqConstraint(t2, t3)

      (t2, Seq(cond, body), Seq(cs1, cs2, cs3))

    case Fix =>
      val (t, cs) = typecheckExpRec(e.kids(0), ctx, tctx)

      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      (X, Seq(fixCons), Seq(cs))

    case TAbs =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val k = if (e.lits.size == 2) e.lits(1).asInstanceOf[Kind] else freshKUVar()
      val (t, cs) = typecheckExpRec(e.kids(0), ctx, tctx + (alpha -> k))
      (TUniv(alpha, Some(k), t), Seq(), Seq(cs))

    case TApp =>
      val (t1, cs) = typecheckExpRec(e.kids(0), ctx, tctx)
      val (k, tcs) = typecheckTypeRec(e.kids(1), tctx)
      val t = Type.from(e.kids(1))

      val Xalpha = freshUVar().x
      val Xbody = freshUVar()
      val Xres = freshUVar()

      val ucons = EqConstraint(UUniv(Xalpha, k, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha.x, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      (Xres, Seq(ucons, vcons), Seq(cs))
  }

  def typecheckTypeStep(e: Node, tctx: TCtx): StepResult[Kind] = e.kind match {
    case TNum.Kind => (KStar, Seq(), Seq())

    case TVar.Kind =>
      val X = e.lits(0).asInstanceOf[Symbol]
      tctx.get(X) match {
        case None => throw UnboundTVariable(X, tctx)
        case Some(k) => (k, Seq(), Seq())
      }

    case TFun.Kind =>
      val (k1, cs1) = typecheckTypeRec(e.kids(0), tctx)
      val (k2, cs2) = typecheckTypeRec(e.kids(0), tctx)
      (KStar, Seq(EqKindConstraint(k1, KStar), EqKindConstraint(k2, KStar)), Seq(cs1, cs2))

    case TUniv.Kind =>
      val X = e.lits(0).asInstanceOf[Symbol]
      val k = if (e.lits.size == 2) e.lits(1).asInstanceOf[Kind] else freshKUVar()
      val (kt, cs) = typecheckTypeRec(e.kids(0), tctx + (X -> k))
      (k, Seq(EqKindConstraint(kt, KStar)), Seq(cs))

    case TTAbs.Kind =>
      val X = e.lits(0).asInstanceOf[Symbol]
      val k = if (e.lits.size == 2) e.lits(1).asInstanceOf[Kind] else freshKUVar()
      val (kt, cs) = typecheckTypeRec(e.kids(0), tctx + (X -> k))
      (KArrow(k, kt), Seq(), Seq(cs))

    case TTApp.Kind =>
      val (k1, cs1) = typecheckTypeRec(e.kids(0), tctx)
      val (k2, cs2) = typecheckTypeRec(e.kids(1), tctx)
      val K = freshKUVar()
      (K, Seq(EqKindConstraint(k1, KArrow(k2, K))), Seq(cs1, cs2))
  }

}

case class UnboundVariable(x: Symbol, ctx: Map[Symbol, Type]) extends RuntimeException
case class UnboundTVariable(x: Symbol, ctx: Map[Symbol, Kind]) extends RuntimeException

case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}