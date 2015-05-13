package incremental.pcf.with_subtyping

import constraints.StatKeys._
import constraints.subtype._
import incremental.Node.Node
import incremental.{Util, Node_}
import incremental.pcf.{Num, Add, Mul, App, Fix, If0, Var}

/**
 * Created by oliver on 20.11.14.
 */
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String
  type Reqs = Map[Symbol, Type]

  type Result = (Type, Reqs, CS)
  type StepResult = (Type, Reqs, Seq[Constraint])

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    localState.stats(TypeCheck) {
      root.visitUninitialized { e =>
        val (t, reqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._3)
        val cs = subcs addNewConstraints cons
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        e.typ = (cs applyPartialSolution t, reqs2, cs.propagate)
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

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case Num =>
      (TInteger, Map(), Seq())
    case op if op == Add || op == Mul =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      (TNumeric, mreqs, mcons :+ Subtype(t1, TNumeric) :+ Subtype(t2, TNumeric))
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = gen.freshUVar(true)
      (X, Map(x -> X), Seq())
    case App =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ
      val X = gen.freshUVar(false)
      val Y = gen.freshUVar(true)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      (Y, mreqs, mcons :+ Equal(t1, TFun(X, Y)) :+ Subtype(t2, X))
    case Abs if e.lits(0).isInstanceOf[Symbol] =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else gen.freshUVar(true)
      val (t, reqs, _) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          (TFun(annotatedT, t), reqs - x, Seq())
        case Some(treq) =>
          val otherReqs = reqs - x
          (TFun(annotatedT, t), otherReqs, Seq(Subtype(annotatedT, treq)))
      }
    case If0 =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ
      val (t3, reqs3, _) = e.kids(2).typ
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)
      val Xjoin = gen.freshUVar(true)
      (Xjoin, mreqs, mcons :+ Subtype(t1, TNumeric) :+ Join(Xjoin, Set(t2, t3)))

    case Fix =>
      val (t, reqs, _) = e.kids(0).typ
      val X = gen.freshUVar(false)
      val Y = gen.freshUVar(true)
      (X, reqs, Seq(Equal(t, TFun(X, Y)), Subtype(Y, X)))
  }


  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    localState.stats(MergeReqs) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    }

  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          val Xmeet = gen.freshUVar(false)
          mcons = Meet(Xmeet, Set(r1, r2)) +: mcons
          mreqs += x -> Xmeet
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