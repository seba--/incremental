package incremental.pcf.with_subtyping

import constraints.StatKeys._
import constraints.subtype._
import incremental.{Node_, Util}
import incremental.Node._
import incremental.pcf.{Num, Add, Mul, App, Fix, If0, Var}

/**
 * Created by oliver on 27.11.14.
 */
abstract class DUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TCtx = Map[Symbol, Type]
  type TError = String
  type Result = (Type, CS)
  type StepResult = (Type, Seq[Constraint], Seq[CS])

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]
    localState.stats(TypeCheck) {
      try {
        val (t, sol_) = typecheckRec(root, Map())
        val sol = sol_.tryFinalize
        if (sol.isSolved)
          Left(t.subst(sol.substitution))
        else
          Right(s"Unresolved constraints ${sol.unsolved}, type ${t.subst(sol.substitution)}, subst ${sol.substitution}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      }
    }
  }

  def typecheckRec(e: Node_[Result], ctx: TCtx): Result = {
    val (t, cons, css) = typecheckStep(e, ctx)
    val subcs = css.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res)
    val cs = subcs addNewConstraints cons
    (cs applyPartialSolution t, cs.propagate)
  }

  def typecheckStep(e: Node_[Result], ctx: TCtx): StepResult = e.kind match {
    case Num =>
      (TInteger, Seq(), Seq())

    case k if k == Add || k == Mul =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      (TNumeric, Seq(Subtype(t1, TNumeric), Subtype(t2, TNumeric)), Seq(cs1, cs2))

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, Seq(), Seq())
      }

    case App =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      val X = gen.freshUVar(false)
      val Y = gen.freshUVar(true)
      (Y, Seq(Equal(TFun(X, Y), t1), Subtype(t2, X)), Seq(cs1, cs2))

    case Abs if e.lits(0).isInstanceOf[Symbol] =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else gen.freshUVar(true)
      val (t, cs) = typecheckRec(e.kids(0), ctx + (x -> annotatedT))
      (TFun(annotatedT, t), Seq(), Seq(cs))

    case If0 =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      val (t3, cs3) = typecheckRec(e.kids(2), ctx)
      val Xjoin = gen.freshUVar(true)
      val sol = Seq(Subtype(t1, TNumeric), Join(Xjoin, Set(t2, t3)))
      (Xjoin, Seq(Subtype(t1, TNumeric), Join(Xjoin, Set(t2, t3))), Seq(cs1, cs2, cs3))

    case Fix =>
      val (t, cs) = typecheckRec(e.kids(0), ctx)
      val X = gen.freshUVar(false)
      val Y = gen.freshUVar(true)
      (X, Seq(Equal(t, TFun(X, Y)), Subtype(Y, X)), Seq(cs))
  }
}

case class UnboundVariable(x: Symbol, ctx: Map[Symbol, Type]) extends RuntimeException

case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}