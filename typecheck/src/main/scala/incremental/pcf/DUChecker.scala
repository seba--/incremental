package incremental.pcf

import constraints.equality.Type.Companion.TSubst
import constraints.equality._
import incremental.Node.Node
import incremental.{Node_, Util}

/**
 * Created by seba on 14/11/14.
 */
abstract class DUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {
  import csFactory._

  type TError = Type.Companion.TError
  type Result = (Type, CS)
  type StepResult = (Type, Seq[Constraint], Seq[CS])

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
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
    )
    localState.stats.typecheckTime += ctime
    res
  }

  def typecheckRec(e: Node_[Result], ctx: TSubst): Result = {
    val (t, cons, css) = typecheckStep(e, ctx)
    val subcs = css.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res)
    val cs = subcs addNewConstraints cons
    (cs applyPartialSolution t, cs.propagate)
  }
  
  def typecheckStep(e: Node_[Result], ctx: TSubst): StepResult = e.kind match {
    case Num => (TNum, Seq(), Seq())
    case k if k == Add || k == Mul =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      
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
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      
      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      
      (X, Seq(fcons), Seq(cs1, cs2))
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X =
        if (e.lits.size == 2)
          e.lits(1).asInstanceOf[Type]
        else
          freshUVar()

      val (t, subsol) = typecheckRec(e.kids(0), ctx + (x -> X))
      (TFun(X, t), Seq(), Seq(subsol))
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshUVar())

      val (t, cs) = typecheckRec(e.kids(0), ctx ++ (xs zip Xs))

      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val X = Xs(i)
        tfun = TFun(X, tfun)
      }

      (tfun, Seq(), Seq(cs))
    case If0 =>
      val (t1, cs1) = typecheckRec(e.kids(0), ctx)
      val (t2, cs2) = typecheckRec(e.kids(1), ctx)
      val (t3, cs3) = typecheckRec(e.kids(2), ctx)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)
      
      (t2, Seq(cond, body), Seq(cs1, cs2, cs3))
    case Fix =>
      val (t, cs) = typecheckRec(e.kids(0), ctx)

      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      
      (X, Seq(fixCons), Seq(cs))
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException

case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}