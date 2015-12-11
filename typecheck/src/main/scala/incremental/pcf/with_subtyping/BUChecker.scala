package incremental.pcf.with_subtyping

import constraints.Statistics
import constraints.subtype._
import incremental.Node.Node
import incremental.{Context, NodeKind, Util, Node_}
import incremental.pcf.with_subtyping.SubtypingCheck._

/**
 * Created by oliver on 20.11.14.
 */

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String

  def typecheckImpl(e: Node[Constraint, Result]): Either[Type, TError] = {
    val root = e.withCS[CS]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val ctx = new Context[Constraint]
        val (t, reqs) = e.kind.check(e.lits, e.kids.seq, ctx)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, subnode) => cs mergeSubsystem subnode.cs)
        val cs = subcs addNewConstraints ctx.getConstraints
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        e.set(cs, (cs applyPartialSolution t, reqs2))
        true
      }

      val (t_, reqs) = root.typ
      val sol_ = root.cs
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
  }
}

case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}