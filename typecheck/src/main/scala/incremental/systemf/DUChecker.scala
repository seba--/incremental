package incremental.systemf

import constraints.Statistics
import constraints.equality._
import incremental.{Util, Node_}
import incremental.Node._


/**
 * Created by seba on 14/11/14.
 */
abstract class DUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String
  type TCtx = Map[Symbol, PolType]
  type Result = (Type, CS)
  type StepResult = (Type, Seq[Constraint], Seq[CS])

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]
    localState.stats.typecheckTimed {
      try {
        val (t, sol_) = typecheckRec(root, Map(), Set())
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
  }

  def typecheckRec(e: Node_[Result], ctx: TCtx, tctx: Set[Symbol]): Result = {
    val (t, cons, css) = typecheckStep(e, ctx, tctx)
    val subcs = css.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res)
    val cs = subcs addNewConstraints cons
    (cs applyPartialSolution t, cs.propagate)
  }

  def typecheckStep(e: Node_[Result], ctx: TCtx, tctx: Set[Symbol]): StepResult = e.kind match {
    case Num => (TNum, Seq(), Seq())
    case k if k == Add || k == Mul =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx, tctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx, tctx)
      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      (TNum, Seq(lcons, rcons), Seq(cs1, cs2))

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, Seq(), Seq())
      }

    case App =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx, tctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx, tctx)
      val X = freshUVar()
      (X, Seq(EqConstraint(TFun(t2, X), t1)), Seq(cs1, cs2))

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X =
        if (e.lits.size == 2) {
          val t = e.lits(1).asInstanceOf[PolType]
          for (x <- t.freeTVars)
            if (!tctx.contains(x))
              throw UnboundTVariable(x, tctx)
          t
        }
        else
          freshUVar()

      val (t, cs) = typecheckRec(e.kids(0), ctx + (x -> X), tctx)
      (TFun(X, t), Seq(), Seq(cs))

    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshUVar())
      val (t, cs) = typecheckRec(e.kids(0), ctx ++ (xs zip Xs), tctx)

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X, tfun)
      }

      (tfun, Seq(), Seq(cs))

    case If0 =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx, tctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx, tctx)
      val (t3, cs3) = typecheckRec(e.kids(2), ctx, tctx)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      (t2, Seq(cond, body), Seq(cs1, cs2, cs3))

    case Fix =>
      val (t, cs) = typecheckRec(e.kids(0), ctx, tctx)

      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      (X, Seq(fixCons), Seq(cs))

    case TAbs if (e.lits(0).isInstanceOf[Symbol]) =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val (t, cs) = typecheckRec(e.kids(0), ctx, tctx + alpha)
      (TUniv(alpha, t), Seq(), Seq(cs))

    case TApp =>
      val (t1, cs) = typecheckRec(e.kids(0), ctx, tctx)
      val t = e.lits(0).asInstanceOf[PolType]

      for (x <- t.freeTVars)
        if (!tctx.contains(x))
          throw UnboundTVariable(x, tctx)

      val Xalpha = freshUVar().x
      val Xbody = freshUVar()
      val Xres = freshUVar()

      val ucons = EqConstraint(UUniv(Xalpha, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha.x, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      (Xres, Seq(ucons, vcons), Seq(cs))
  }
}

case class UnboundVariable(x: Symbol, ctx: Map[Symbol, PolType]) extends RuntimeException
case class UnboundTVariable(x: Symbol, ctx: Set[Symbol]) extends RuntimeException

case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}