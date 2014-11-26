package incremental.pcf.with_subtyping

import scala.language.implicitConversions
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type._
import incremental._
import incremental.pcf.{Constraint => _, _}
import incremental.pcf.with_subtyping.Constraints._
import incremental.pcf.with_subtyping.TypeOps._


/**
 * Created by oliver on 20.11.14.
 */
class BottomUpChecker extends TypeChecker {
  var preparationTime = 0.0
  var typecheckTime = 0.0

  val solver = new Solver
  import solver._
  def constraintCount = solver.constraintCount
  def mergeReqsTime = solver.mergeReqsTime
  def constraintSolveTime = solver.constraintSolveTime

  type Result = (Type, Require, CSet)

  def typecheck(e: Exp): Either[Type, TError] = {
    Constraints.reset()
    val root = e.withType[Result]

    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    preparationTime += ptime

    val (res, ctime) = Util.timed {
      uninitialized foreach (e => if (!e.valid) typecheckSpine(e))

      val (t, reqs, unres) = root.typ
      val sigma = unres.finalized

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres $unres")
      else if (!unres.isEmpty)
        Right(s"Unresolved constraints $unres, type $t")
      else
        Left(t)
    }
    typecheckTime += ctime
    res
  }

  def typecheckSpine(e: Exp_[Result]): Unit = {
    var current = e
    while (current != null && current.allKidTypesAvailable) {
      val isFirstTime = !current.valid
      val isRoot = current.parent == null

      val t = typecheckStep(current)
      //      println(s"$current -> t")
      //      println(s"  old: ${current.typ}")

      current.typ = t
      if (!isRoot && isFirstTime)
        current.parent.markKidTypeAvailable(current.pos)
      current = current.parent
    }
  }

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Num => (TNum, Map(), empty)
    case op if op == Add || op == Mul =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ
      val lcons = Constraint.normalizeEq(t1, TNum) //TODO add some more interesting use of subtyping here?
      val rcons = Constraint.normalizeEq(t2, TNum)
      val (mreqs, mcons) = mergeReqMaps(reqs1, reqs2)
      val (s, newunres) = solve(unres1 && unres2 && lcons && rcons && mcons)
      (TNum, mreqs.mapValues(_.subst(s)), newunres)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshTVar()
      (X, Map(x -> X), empty)
    case App =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ
      val (u, v) = (freshTVar(), freshTVar())
      val fcons = Constraint.normalizeEq(t1, u -->: v)
      val argcons = Constraint.normalizeSub(Bot, t2, u)
      val (mreqs, mcons) = mergeReqMaps(reqs1, reqs2)
      val (s, newunres) = solve(unres1 && unres2 && fcons && argcons && mcons)
      (v.subst(s), mreqs.mapValues(_.subst(s)), newunres)
    case Abs if (e.lits(0).isInstanceOf[Symbol] && e.lits(1).isInstanceOf[Type]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = e.lits(1).asInstanceOf[Type]
      val (t, reqs, unres) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          (annotatedT -->: t, reqs - x, unres)
        case Some(treq) =>
          val otherReqs = reqs - x
          val xcons = Constraint.normalizeEq(treq, annotatedT)
          val (s, newunres)  = solve(unres && xcons)
          ((annotatedT -->: t).subst(s), otherReqs.mapValues(_.subst(s)), newunres)
      }
    /*case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, unres) = e.kids(0).typ

      val Xs = xs map (_ => freshTVar())

      var restReqs = reqs
      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val x = xs(i)
        restReqs.get(x) match {
          case None =>
            val X = freshTVar()
            tfun = TFun(X, tfun)
          case Some(treq) =>
            restReqs = restReqs - x
            tfun = TFun(treq, tfun)
        }
      }

      (tfun, restReqs, unres)*/
    case If0 =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ
      val (t3, reqs3, unres3) = e.kids(2).typ
      val (reqsCombined, cset) = mergeReqMaps(reqs1, reqs2, reqs3)
      val t4 = freshTVar()
      val ccons = Constraint.normalizeEq(t1, TNum)
      val tcons = Constraint.normalizeSub(Bot, t2, t4)
      val econs = Constraint.normalizeSub(Bot, t3, t4)
      val (s, newunres) = solve(unres1 && unres2 && unres3 && cset && ccons && tcons && econs)
      (t4.subst(s), reqsCombined.mapValues(_.subst(s)), newunres)
    case Fix =>
      val (t, reqs, unres) = e.kids(0).typ
      val X = freshTVar()
      val fixCons = Constraint.normalizeEq(t, X -->: X)
      val (s, newunres) = solve(unres && fixCons)
      (X.subst(s), reqs.mapValues(_.subst(s)), newunres)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new BottomUpChecker
}