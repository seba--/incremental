package incremental.pcf

import constraints.Statistics
import constraints.equality._
import incremental.{Context, Node_, Util}
import incremental.Node.Node
import PCFCheck._

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {
  import csFactory._

  type TError = String

  def typecheckImpl(e: Node[Constraint, Result]): Either[Type, TError] = {
    val root = e.withCS[CS]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        typecheckRec(e)
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

  def typecheckRec(e: Node_[Constraint, CS, Result]): Unit = {
    val ctx = new Context[Constraint]
    val res@(t, reqs) = e.kind.check(e.lits, e.kids.seq, ctx)
    val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, subnode) => cs mergeSubsystem subnode.cs)
    val cs = subcs addNewConstraints ctx.getConstraints
    val reqs2 = cs.applyPartialSolutionIt[(Symbol, T), Map[Symbol, T], T](reqs, p => p._2)
    e.set(cs.propagate, (cs applyPartialSolution t, reqs2).asInstanceOf[Result]) // TODO: get rid of instanceOf
  }
}



case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}