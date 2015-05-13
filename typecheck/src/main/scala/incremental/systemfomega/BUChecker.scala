package incremental.systemfomega

import constraints.StatKeys._
import constraints.normequality._
import incremental.Node._
import incremental.{Node_, Util}

/**
 * Created by seba on 13/11/14.
 */
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String

  type Reqs = Map[Symbol, Type]
  type TReqs = Map[Symbol, Kind]

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

    localState.stats(TypeCheck) {
      root.visitUninitialized {e =>
        if (e.kind.isInstanceOf[Exp]) {
          val ExpStepResult(t, reqs, treqs, cons) = typecheckExpStep(e)
          val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ.cs)
          val cs = subcs addNewConstraints cons
          val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
          val treqs2 = cs.applyPartialSolutionIt[(Symbol, Kind), Map[Symbol, Kind], Kind](treqs, p => p._2)
          e.typ = ExpResult(cs applyPartialSolution t, reqs2, treqs2, cs.propagate)
          true
        }
        else if (e.kind.isInstanceOf[Type.Kind]) {
          val TypeStepResult(k, treqs, cons) = typecheckTypeStep(e)
          val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ.cs)
          val cs = subcs addNewConstraints cons
          val treqs2 = cs.applyPartialSolutionIt[(Symbol, Kind), Map[Symbol, Kind], Kind](treqs, p => p._2)
          e.typ = TypeResult(k, treqs2, cs.propagate)
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
  }

  def typecheckExpStep(e: Node_[Result]): ExpStepResult = e.kind match {
    case Num => ExpStepResult(TNum(), Map(), Map(), Seq())

    case op if op == Add || op == Mul =>
      val ExpResult(t1, reqs1, treqs1, _) = e.kids(0).typ
      val ExpResult(t2, reqs2, treqs2, _) = e.kids(1).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)
      val lcons = EqConstraint(TNum(), t1)
      val rcons = EqConstraint(TNum(), t2)

      ExpStepResult(TNum(), mreqs, mtreqs, mcons ++ mtcons :+ lcons :+ rcons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      ExpStepResult(X, Map(x -> X), Map(), Seq())

    case App =>

      val ExpResult(t1, reqs1, treqs1, _) = e.kids(0).typ
      val ExpResult(t2, reqs2, treqs2, _) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)

      ExpStepResult(X, mreqs, mtreqs, mcons ++ mtcons :+ fcons)

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]

      if (e.kids(0).kind.isInstanceOf[Type.Kind]) {
        val TypeResult(k, treqs1, _) = e.kids(0).typ
        val T = Type.from(e.kids(0))
        val ExpResult(t, reqs, treqs2, _) = e.kids(1).typ

        val kcons = EqKindConstraint(k, KStar)
        val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)

        reqs.get(x) match {
          case None => ExpStepResult(TFun(T, t), reqs, mtreqs, mtcons :+ kcons)
          case Some(treq) => ExpStepResult(TFun(T, t), reqs - x, mtreqs, mtcons :+ kcons :+ EqConstraint(T, treq))
        }
      }
      else {
        val ExpResult(t, reqs, treqs, _) = e.kids(0).typ

        reqs.get(x) match {
          case None => ExpStepResult(TFun(freshUVar(), t), reqs, treqs, Seq())
          case Some(treq) => ExpStepResult(TFun(treq, t), reqs - x, treqs, Seq())
        }
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
      val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2, treqs3)

      val cond = EqConstraint(TNum(), t1)
      val body = EqConstraint(t2, t3)

      ExpStepResult(t2, mreqs, mtreqs, mcons ++ mtcons :+ cond :+ body)

    case Fix =>
      val ExpResult(t, reqs, treqs, _) = e.kids(0).typ
      val X = freshUVar()
      ExpStepResult(X, reqs, treqs, Seq(EqConstraint(t, TFun(X, X))))

    case TAbs =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val k = if (e.lits.size == 2) e.lits(1).asInstanceOf[Kind] else freshKUVar()
      val ExpResult(t, reqs, treqs, _) = e.kids(0).typ

      treqs.get(alpha) match {
        case None => ExpStepResult(TUniv(alpha, Some(k), t), reqs, treqs, Seq())
        case Some(k2) => ExpStepResult(TUniv(alpha, Some(k), t), reqs, treqs - alpha, Seq(EqKindConstraint(k, k2)))
      }

    case TApp =>
      val ExpResult(t1, reqs1, treqs1, _) = e.kids(0).typ
      val TypeResult(k, treqs2, _) = e.kids(1).typ
      val t = Type.from(e.kids(1))

      val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)

      val Xalpha = freshUVar().x
      val Xbody = freshUVar()
      val Xres = freshUVar()

      val ucons = EqConstraint(UUniv(Xalpha, k, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha.x, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      ExpStepResult(Xres, reqs1, mtreqs, mtcons :+ ucons :+ vcons)
  }


  def typecheckTypeStep(e: Node_[Result]): TypeStepResult = e.kind match {
    case TNum.Kind => TypeStepResult(KStar, Map(), Seq())

    case TVar.Kind =>
      val X = e.lits(0).asInstanceOf[Symbol]
      val K = freshKUVar()
      TypeStepResult(K, Map(X -> K), Seq())

    case TFun.Kind =>
      val TypeResult(k1, treqs1, _) = e.kids(0).typ
      val TypeResult(k2, treqs2, _) = e.kids(1).typ
      val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)
      TypeStepResult(KStar, mtreqs, mtcons :+ EqKindConstraint(k1, KStar) :+ EqKindConstraint(k2, KStar))

    case TUniv.Kind =>
      val X = e.lits(0).asInstanceOf[Symbol]
      val TypeResult(kt, treqs, _) = e.kids(0).typ
      val ktc = EqKindConstraint(kt, KStar)

      if (e.lits.size == 2) {
        val k = e.lits(1).asInstanceOf[Kind]

        treqs.get(X) match {
          case None => TypeStepResult(k, treqs, Seq(ktc))
          case Some(k2) =>
            TypeStepResult(k, treqs - X, Seq(ktc, EqKindConstraint(k, k2)))
        }
      }
      else {
        treqs.get(X) match {
          case None => TypeStepResult(freshKUVar(), treqs, Seq(ktc))
          case Some(k2) =>
            TypeStepResult(k2, treqs - X, Seq(ktc))
        }
      }
  }

  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())
  private val tinit: (Seq[Constraint], TReqs) = (Seq(), Map())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)
  def mergeTReqMaps(req: TReqs, reqs: TReqs*): (Seq[Constraint], TReqs) = mergeTReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    localState.stats(MergeReqs) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps(EqConstraint))
    }

  def mergeTReqMaps(reqs: Seq[TReqs]): (Seq[Constraint], TReqs) =
    localState.stats(MergeReqs) {
      reqs.foldLeft[(Seq[Constraint], TReqs)](tinit)(_mergeReqMaps(EqKindConstraint))
    }

  private def _mergeReqMaps[K, V](eqC: (V,V) => Constraint)(was: (Seq[Constraint], Map[K,V]), newReqs: Map[K,V]) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = eqC(r1, r2) +: mcons
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