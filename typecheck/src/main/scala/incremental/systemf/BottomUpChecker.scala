package incremental.systemf

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Type._
import incremental.Exp._
import incremental._

/**
 * Created by seba on 13/11/14.
 */
class BottomUpChecker extends TypeChecker {

  val constraint = new ConstraintOps
  import constraint._

  var preparationTime = 0.0
  var typecheckTime = 0.0
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime

  type Reqs = Map[Symbol, Type]

  type Result = (Type, Reqs, Solution)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    preparationTime += ptime

    val (res, ctime) = Util.timed {
      uninitialized foreach (e => if (!e.valid) typecheckSpine(e))

      val (t_, reqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.solution)
      
      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
    typecheckTime += ctime
    res
  }

  def typecheckSpine(e: Exp_[Result]): Unit ={
    var current = e
    while (current != null && current.allKidTypesAvailable) {
      val isFirstTime = !current.valid
      val isRoot = current.parent == null

      val t = typecheckStep(current)
      println(s"$current -> $t")
//      println(s"  old: ${current.typ}")

      current.typ = t
      if (!isRoot && isFirstTime)
        current.parent.markKidTypeAvailable(current.pos)
      current = current.parent
    }
  }

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Num => (TNum, Map(), emptySol)
    case op if op == Add || op == Mul =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = solve(lcons +: rcons +: mcons)
      (TNum, mreqs.mapValues(_.subst(sol.solution)), sol1 +++ sol2 <++ sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshTVar()
      (X, Map(x -> X), emptySol)
    case App =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ

      val X = freshTVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = solve(fcons +: mcons)
      (X.subst(sol.solution), mreqs.mapValues(_.subst(sol.solution)), sol1 +++ sol2 <++ sol)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, subsol) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshTVar()
          (TFun(X, t), reqs, subsol)
        case Some(treq) =>
          val otherReqs = reqs - x
          if (e.lits.size == 2) {
            val sol = solve(EqConstraint(e.lits(1).asInstanceOf[Type], treq))
            (TFun(treq, t).subst(sol.solution), otherReqs.mapValues(_.subst(sol.solution)), subsol <++ sol)
          }
          else
            (TFun(treq, t), otherReqs, subsol)
      }
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, subsol) = e.kids(0).typ

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

      (tfun, restReqs, subsol)
    case If0 =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val (t3, reqs3, sol3) = e.kids(2).typ

      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      val sol = solve(cond +: body +: (mcons12 ++ mcons23))

      (t2.subst(sol.solution), mreqs123.mapValues(_.subst(sol.solution)), sol1 +++ sol2 +++ sol3 <++ sol)

    case Fix =>
      val (t, reqs, subsol) = e.kids(0).typ
      val X = freshTVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = solve(fixCons)
      (X.subst(sol.solution), reqs.mapValues(_.subst(sol.solution)), subsol <++ sol)


    case TAbs if (e.lits(0).isInstanceOf[Symbol]) =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, subsol) = e.kids(0).typ
      (TUniv(alpha, t), reqs - alpha, subsol)

    case TApp =>
      val (t1, reqs1, subsol) = e.kids(0).typ
      val t = e.lits(0).asInstanceOf[Type]

      val Xalpha = freshTVar().x
      val Xbody = freshTVar()
      val Xres = freshTVar()

      val ucons = EqConstraint(TUnivInternal(Xalpha, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      val sol = solve(Seq(ucons, vcons))

      (Xres.subst(sol.solution), reqs1.mapValues(_.subst(sol.solution)), subsol <++ sol)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new BottomUpChecker
}