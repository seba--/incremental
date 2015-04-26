package incremental.pcf

import constraints.equality
import constraints.equality.{ConstraintSystem, EqConstraint, Type}
import constraints.equality.ConstraintSystemFactory._
import incremental.{TypeCheckerFactory, Node_, Util}
import incremental.Node.Node

abstract class BUTypeChecker extends incremental.TypeChecker[equality.Type, equality.UVar] {

  type TError = Type.Companion.TError
  type Reqs = Map[Symbol, Type]
  type Constraint = EqConstraint
//  type CS = ConstraintSystem
//  type CSFactory = ConstraintSystemFactory.type
//  val csFactory = ConstraintSystemFactory

  type StepResult = (Type, Reqs, Seq[Constraint])
  type Result = (Type, Reqs, ConstraintSystem)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    val (res, ctime) = Util.timed {
      root.visitUninitialized {e =>
        val (t, reqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._3)
        val cs = subcs addNewConstraints cons
        e.typ = (cs applyPartialSolution t, reqs.mapValues(cs applyPartialSolution _), cs.propagate)
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
    localState.stats.typecheckTime += ctime
    res
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case Num => (TNum, Map(), Seq())
    case op if op == Add || op == Mul =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (TNum, mreqs, mcons :+ lcons :+ rcons)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> X), Seq())
    case App =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      (X, mreqs, mcons :+ fcons)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, _) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
          (TFun(X, t), reqs, Seq())
        case Some(treq) =>
          val otherReqs = reqs - x
          if (e.lits.size == 2) {
            (TFun(treq, t), otherReqs, Seq(EqConstraint(e.lits(1).asInstanceOf[Type], treq)))
          }
          else
            (TFun(treq, t), otherReqs, Seq())
      }
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, _) = e.kids(0).typ

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

      (tfun, restReqs, Seq())
    case If0 =>
      val (t1, reqs1, _) = e.kids(0).typ
      val (t2, reqs2, _) = e.kids(1).typ
      val (t3, reqs3, _) = e.kids(2).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)
      
      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      (t2, mreqs, mcons :+ cond :+ body)

    case Fix =>
      val (t, reqs, _) = e.kids(0).typ
      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      (X, reqs, Seq(fixCons))
  }



  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())

  def mergeReqMaps(reqs: Reqs*) = {
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

trait BUTypeCheckerFactory extends TypeCheckerFactory[equality.Type, equality.UVar]