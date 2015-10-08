package incremental.spreadsheet

import constraints.Statistics
import constraints.equality._
import incremental.{Node_, Util}
import incremental.Node.Node

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {
  import csFactory._

  type TError = String
  type Reqs = Map[String, Map[Int, Type]]

  type StepResult = (Type, Reqs, Seq[Constraint])
  type Result = (Type, Reqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        typecheckRec(e)
        true
      }

      val (t_, reqs, sol_) = root.typ
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

  def typecheckRec(e: Node_[Result]): Unit = {
    val res@(t, reqs, cons) = typecheckStep(e)
    val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._3)
    val cs = subcs addNewConstraints cons
    // TODO: apply partial solution to reqs
//    val reqs2 = cs.applyPartialSolutionIt[T, Iterable[T], T](reqs.values.flatMap(_.values), p => p)
    e.typ = (cs applyPartialSolution t, reqs, cs.propagate)
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case _ => ???
  }



  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    }

  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) => {
          var rowchange = false
          var newrow = r1
          for ((y, t2) <- r2)
            r1.get(y) match {
              case None => {
                newrow += y -> t2
                rowchange = true
              }
              case Some(t1) =>
                mcons = mcons :+ EqConstraint(t1, t2)
            }
          if (rowchange)
            mreqs += x -> newrow
        }
      }
    (mcons, mreqs)
  }
}



case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}