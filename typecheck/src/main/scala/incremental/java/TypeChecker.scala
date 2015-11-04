package incremental.java

import constraints.Statistics
import constraints.javacons._
import incremental.{Util, Node_, MyBuilder}
import incremental.java.syntax.{UVar, Type}
import incremental.java.JavaCheck._
import incremental.Node.Node

import scala.collection.generic.CanBuildFrom

/**
 * Created by qwert on 22.07.15.
 */

abstract class TypeChecker[CS <: ConstraintSystem[CS]] extends incremental.TypeChecker[Gen, Constraint, CS]{
  type T = Type
  type CSFactory <: ConstraintSystemFactory[CS]
  implicit val csFactory: CSFactory

  implicit def bf[K, V] = new CanBuildFrom[Iterable[(K, V)], ((K, V), V), Map[K, V]] {
    def apply = new MyBuilder
    def apply(from: Iterable[(K, V)]) = from.foldLeft(new MyBuilder[K, V])((b, p) => b += ((p) -> p._2))
  }

  def freshUVar() = UVar(freshSymbol("x$"))
}

trait TypeCheckerFactory[CS <: ConstraintSystem[CS]] {
  def makeChecker: TypeChecker[CS]
}

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {
  import csFactory._

  type TError = String
  //type Result = (CheckRes, VReqs, CReqs, CS)

  def typecheckImpl(e: Node): Either[CheckRes, TError] = { // TODO: CheckRes -> Type ?
    val root = e.withTypes[CS, Result]


    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        typecheckRec(e)
        true
      }

      val (tRes, vReqs, cReqs) = root.typ
      val sol_ = root.cs
      val sol = sol_.tryFinalize
      // TODO: val t: Type, but tRes is CheckRes

      if(vReqs.nonEmpty)
        Right(s"Unresolved variable requirements $vReqs, type $tRes")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints")
      else
        Left(tRes)
    }
  }

  def typecheckRec(e: Node_[CS, Result]): Unit = {
    // TODO: res must be StepResult, kids are Result?
    val res@(t, vReqs, cReqs) = e.kind.check(e.lits, e.kids.seq, JavaContext)
    val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
    val cs = ??? //subcs addNewConstraints cons

    e.set(cs, (t, vReqs, cReqs)) // TODO: apply (partial) solution (mgu)
  }
}