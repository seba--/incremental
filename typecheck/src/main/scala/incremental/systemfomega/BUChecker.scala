package incremental.systemfomega

import constraints.Statistics
import constraints.normequality._
import incremental.Node._
import incremental.{Context, Node_, Util}
import incremental.systemfomega.OmegaCheck._

/**
 * Created by seba on 13/11/14.
 */
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String

  //trait StepResult
  //case class ExpStepResult(t: Type, reqs: Reqs, treqs: TReqs, cons: Seq[Constraint]) extends StepResult
  //case class TypeStepResult(k: Kind, treqs: TReqs, cons: Seq[Constraint]) extends StepResult

  def typecheckImpl(e: Node[Constraint, Result]): Either[Type, TError] = {
    val root = e.withCS[CS]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        if (e.kind.isInstanceOf[Exp]) {
          val ctx = new Context[Constraint]
          val ExpResult(t, reqs, treqs) = e.kind.check(e.lits, e.kids.seq, ctx)
          val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, subnode) => cs mergeSubsystem subnode.cs)
          val cs = subcs addNewConstraints ctx.getConstraints
          val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
          val treqs2 = cs.applyPartialSolutionIt[(Symbol, Kind), Map[Symbol, Kind], Kind](treqs, p => p._2)
          //e.typ = ExpResult(cs applyPartialSolution t, reqs2, treqs2, cs.propagate)
          e.set(cs.propagate, ExpResult(cs applyPartialSolution t, reqs2, treqs2))
          true
        }
        else if (e.kind.isInstanceOf[Type.Kind]) {
          val ctx = new Context[Constraint]
          val TypeResult(k, treqs) = e.kind.check(e.lits, e.kids.seq, ctx)
          val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, subnode) => cs mergeSubsystem subnode.cs)
          val cs = subcs addNewConstraints ctx.getConstraints
          val treqs2 = cs.applyPartialSolutionIt[(Symbol, Kind), Map[Symbol, Kind], Kind](treqs, p => p._2)
          //e.typ = TypeResult(k, treqs2, cs.propagate)
          e.set(cs.propagate, TypeResult(k, treqs2))
          true
        }
        else
          throw new MatchError(s"Unsupported node kind ${e.kind}")
      }

      val ExpResult(t_, reqs, treqs) = root.typ
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