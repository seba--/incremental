package incremental.systemfomega

import constraints.normequality._
import incremental.Node._
import incremental.{Node_, Util}

/**
 * Created by seba on 13/11/14.
 */
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = Type.Companion.TError

  type Reqs = Map[Symbol, Type]
  type TReqs = Set[Symbol]

  trait StepResult
  case class ExpStepResult(t: Type, reqs: Reqs, treqs: TReqs, cons: Seq[Constraint]) extends StepResult
  case class TypeStepResult(k: Kind, treqs: TReqs, cons: Seq[Constraint]) extends StepResult

  trait Result {
    val cs: CS
  }
  case class ExpResult(t: Type, reqs: Reqs, treqs: TReqs, cs: CS) extends Result
  case class TypeResult(k: Kind, treqs: TReqs, cs: CS) extends Result

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    val (res, ctime) = Util.timed {
      root.visitUninitialized {e =>
        if (e.kind.isInstanceOf[Exp]) {
          val ExpStepResult(t, reqs, treqs, cons) = typecheckExpStep(e)
          val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ.cs)
          val cs = subcs addNewConstraints cons
          val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type]](reqs, p => p._2)
          e.typ = ExpResult(cs applyPartialSolution t, reqs2, treqs, cs.propagate)
          true
        }
        else if (e.kind.isInstanceOf[Type.Kind]) {
          val TypeStepResult(k, treqs, cons) = typecheckTypeStep(e)
          val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ.cs)
          val cs = subcs addNewConstraints cons
          e.typ = TypeResult(k, treqs, cs.propagate)
          true
        }
        else
          throw new MatchError(s"Unsupported node kind ${e.kind}")
      }

      val ExpResult(t_, reqs, treqs, cs_) = root.typ
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
    localState.stats.typecheckTime += ctime
    res
  }

  def typecheckExpStep(e: Node_[Result]): ExpStepResult = e.kind match {
    case Num => ExpStepResult(TNum, Map(), Set(), Seq())
    case op if op == Add || op == Mul =>
      val ExpResult(t1, reqs1, treqs1, _) = e.kids(0).typ
      val ExpResult(t2, reqs2, treqs2, _) = e.kids(1).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      ExpStepResult(TNum, mreqs, treqs1 ++ treqs2, mcons :+ lcons :+ rcons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      ExpStepResult(X, Map(x -> X), Set(), Seq())

    case App =>

      val ExpResult(t1, reqs1, treqs1, _) = e.kids(0).typ
      val ExpResult(t2, reqs2, treqs2, _) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      ExpStepResult(X, mreqs, treqs1 ++ treqs2, mcons :+ fcons)

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val ExpResult(t, reqs, treqs, _) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
          ExpStepResult(TFun(X, t), reqs, treqs ++ X.freeTVars, Seq())
        case Some(treq) =>
          val otherReqs = reqs - x
          if (e.lits.size == 2) {
            val X = e.lits(1).asInstanceOf[Type]
            ExpStepResult(TFun(treq, t), otherReqs, treqs ++ X.freeTVars, Seq(EqConstraint(X, treq)))
          }
          else
            ExpStepResult(TFun(treq, t), otherReqs, treqs, Seq())
      }
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val ExpResult(t, reqs, treqs, _) = e.kids(0).typ

      val Xs = xs map (_ => freshUVar())

      var restReqs = reqs
      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val x = xs(i)
        restReqs.get(x) match {
          case None =>
            val X = freshUVar()
            tfun = TFun(X, tfun)
          case Some(treq) =>
            restReqs = restReqs - x
            tfun = TFun(treq, tfun)
        }
      }

      ExpStepResult(tfun, restReqs, treqs, Seq())

    case If0 =>
      val ExpResult(t1, reqs1, treqs1, _) = e.kids(0).typ
      val ExpResult(t2, reqs2, treqs2, _) = e.kids(1).typ
      val ExpResult(t3, reqs3, treqs3, _) = e.kids(2).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      ExpStepResult(t2, mreqs, treqs1 ++ treqs2 ++ treqs3, mcons :+ cond :+ body)

    case Fix =>
      val ExpResult(t, reqs, treqs, _) = e.kids(0).typ
      val X = freshUVar()
      ExpStepResult(X, reqs, treqs, Seq(EqConstraint(t, TFun(X, X))))

    case TAbs =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val ExpResult(t, reqs, treqs, _) = e.kids(0).typ
      ExpStepResult(TUniv(alpha, t), reqs, treqs - alpha, Seq())

    case TApp =>
      val ExpResult(t1, reqs1, treqs, _) = e.kids(0).typ
      val t = e.lits(0).asInstanceOf[Type]

      val Xalpha = freshUVar().x
      val Xbody = freshUVar()
      val Xres = freshUVar()

      val ucons = EqConstraint(UUniv(Xalpha, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      ExpStepResult(Xres, reqs1, treqs ++ t.freeTVars, Seq(ucons, vcons))
  }


  def typecheckTypeStep(e: Node_[Result]): TypeStepResult = e.kind match {
    case TNum.Kind => TypeStepResult(KStar, Set(), Seq())
    case TVar.Kind =>
      // TODO
      TypeStepResult(KStar, Set(), Seq())
  }

  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) = {
    val (res, time) = Util.timed{
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    }
    localState.stats.mergeReqsTime += time
    res
  }

  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = EqConstraint(r1, r2) +: mcons
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