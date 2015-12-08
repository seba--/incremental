package incremental.systemf

import constraints.Statistics
import constraints.equality._
import incremental.Node._
import incremental.{Context, Node_, Util}
import incremental.systemf.SystemFCheck._

/**
 * Created by seba on 13/11/14.
 */
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String

  def typecheckImpl(e: Node[Constraint, Result]): Either[Type, TError] = {
    val root = e.withCS[CS]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        val ctx = new Context[Constraint]
        val (t, reqs, treqs) = e.kind.check(e.lits, e.kids.seq, ctx)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, subnode) => cs mergeSubsystem subnode.cs)
        val cs = subcs addNewConstraints ctx.getConstraints
        val reqs2 = cs.applyPartialSolutionIt[(Symbol,Type),Map[Symbol,Type],Type](reqs, p => p._2)
        e.set(cs.propagate, (cs applyPartialSolution t, reqs2, treqs))
        true
      }

      val (t_, reqs, treqs) = root.typ
      val cs_ = root.cs
      val cs = cs_.tryFinalize
      val t = t_.subst(cs.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved variable requirements $reqs, type $t, unres ${cs.unsolved}")
      else if (!treqs.isEmpty)
        Right(s"Unresolved type-variable requirements $treqs, type $t, unres ${cs.unsolved}")
      else if (!cs.isSolved)
        Right(s"Unresolved constraints ${cs.unsolved}, type $t")
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