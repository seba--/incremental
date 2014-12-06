package incremental.pcf.with_subtyping

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.pcf._
import incremental.{TypeCheckerFactory, Exp_, TypeChecker, Util}
import incremental.pcf.with_subtyping.Type.Companion
import TypeOps._

/**
 * Created by oliver on 20.11.14.
 */
class BottomUpChecker extends TypeChecker[Type] {
  var preparationTime = 0.0
  var typecheckTime = 0.0

  val constraint = new Constr
  import constraint._
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime
  def mergeSolutionTime = constraint.mergeSolutionTime

  type Reqs = Map[Symbol, Type]

  type Result = (Type, Reqs, CSet)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    preparationTime += ptime

    val (res, ctime) = Util.timed {
      uninitialized foreach (e => if (!e.valid) typecheckSpine(e))

      val (t_, reqs, sol_) = root.typ
      val (sigma, notyet, unsat) = sol_.tryFinalize.state
      val t = t_.subst(sigma)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${notyet}")
      else if (!(notyet.isEmpty && unsat.isEmpty))
        Right(s"Unresolved constraints notyet: $notyet\nunsat: ${unsat}, type $t")
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
    case Num =>
      (TNum, Map(), CSet())
    case op if op == Add || op == Mul =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val sol = sol1 <-- sol2 <-- Equal(TNum, t1) <-- Equal(TNum, t2) <-- mcons
      (TNum, mreqs.mapValues(_.subst(sol.solution)), sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshTVar()
      (X, Map(x -> X), CSet())
    case App =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val X = freshTVar(false)
      val Y = freshTVar()
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val sol = sol1 <-- sol2 + Equal(t1, X -->: Y) <-- Subtype(t2, X) <-- mcons
      (Y.subst(sol.solution), mreqs.mapValues(_.subst(sol.solution)), sol)
    case Abs if e.lits(0).isInstanceOf[Symbol] =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshTVar()
      val (t, reqs, subsol) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          (TFun(annotatedT, t), reqs - x, subsol)
        case Some(treq) =>
          val otherReqs = reqs - x
          val sol = subsol <-- Subtype(annotatedT, treq)
          (TFun(annotatedT, t).subst(sol.solution), otherReqs.mapValues(_.subst(sol.solution)), sol)
      }
    case If0 =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val (t3, reqs3, sol3) = e.kids(2).typ
      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)
      val Xjoin = freshTVar()
      val sol = sol1 <-- sol2 <-- sol3 + Equal(TNum, t1) + Subtype(t2, Xjoin) <-- Subtype(t3, Xjoin) <-- mcons12 <-- mcons23
      (Xjoin.subst(sol.solution), mreqs123.mapValues(_.subst(sol.solution)), sol)

    case Fix =>
      val (t, reqs, subsol) = e.kids(0).typ
      val X = freshTVar(false)
      val Y = freshTVar()
      val sol = subsol + Equal(t, X -->: Y) <-- Subtype(Y, X)
      (X.subst(sol.solution), reqs.mapValues(_.subst(sol.solution)), sol)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}