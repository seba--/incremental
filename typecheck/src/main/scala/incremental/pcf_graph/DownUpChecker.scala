package incremental.pcf_graph

import incremental.ConstraintOps._
import incremental.Node.Node
import incremental.Node._
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

  def typecheck(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]
    val (res, ctime) = Util.timed(
      try {
        val (t, sol_) = typecheck(root, Map())
        val sol = sol_.tryFinalize
        if (sol.isSolved)
          Left(t.normalize)
        else
          Right(s"Unresolved constraints ${sol.unsolved}, type ${t.normalize}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      }
    )
    typecheckTime += ctime
    res
  }

  def typecheck(e: Node_[Result], ctx: TSubst): Result = e.kind match {
    case Num => (TNum, emptySol)
    case k if k == Add || k == Mul =>
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
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
      val (t1, sol1) = typecheck(e.kids(0), ctx)
      val (t2, sol2) = typecheck(e.kids(1), ctx)
      val subsol = sol1 ++ sol2

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val sol = solve(fcons, subsol)

      (X, sol)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X =
        if (e.lits.size == 2)
          e.lits(1).asInstanceOf[Type]
        else
          freshUVar()

      val (t, subsol) = typecheck(e.kids(0), ctx + (x -> X))
      (TFun(X, t), subsol)
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val Xs = xs map (_ => freshUVar())

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
      val sol = solve(Seq(cond, body), subsol)

      (t2, sol)
    case Fix =>
      val (t, subsol) = typecheck(e.kids(0), ctx)

      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = solve(fixCons, subsol)

      (X, sol)
  }
}

case class UnboundVariable(x: Symbol, ctx: TSubst) extends RuntimeException

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new DownUpChecker
}