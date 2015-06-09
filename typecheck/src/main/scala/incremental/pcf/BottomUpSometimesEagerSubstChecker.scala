package incremental.pcf

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Type.Companion
import incremental.Type.Companion._
import incremental.Exp._
import incremental._

import scala.io.StdIn

/**
 * Created by seba on 13/11/14.
 */
class BottomUpSometimesEagerSubstChecker(SUBST_THRESHOLD: Int) extends BUChecker[Type] {
  type CSystem = ConstraintOps.type
  val cs = ConstraintOps
  import cs._
  import localState.gen._

  override def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    //    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    //    preparationTime += ptime
    cs.state.withValue(localState) {
      val (res, ctime) = Util.timed {
        root.visitUninitialized { e =>
          e.typ = normalizedTypecheckStep(e)
          true
        }

        val (t_, reqs, sol_) = solvePartially(root.typ)
        println(s"t: $t_ \nreqs: $reqs \nsol: ${sol_.substitution}")
        println("Enter to finalize")
        StdIn.readLine()
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
  }

  def normalizedTypecheckStep(e: Exp_[Result]) = {
    val res = typecheckStep(e)
    val sol = res._3
    val s = sol.substitution
    if (s.size > SUBST_THRESHOLD)
      solvePartially(res)
    else
      res
  }


  def solvePartially(res: Result): Result = {
    val sol = res._3
    val s = sol.substitution
    (res._1.subst(s), res._2.mapValues(_.subst(s)), CSet(Map(), sol.notyet.map(_.subst(s)), sol.never.map(_.subst(s))))
  }

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Num => (TNum, Map(), emptyCSet)
    case op if op == Add || op == Mul =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val subsol = sol1 ++++ sol2

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = subsol ++ (Seq(lcons, rcons) ++ mcons)
      (TNum, mreqs, sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> X), emptyCSet)
    case App =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val subsol = sol1 ++++ sol2

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = subsol ++ (fcons +: mcons)
      (X, mreqs, sol)
    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, subsol) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
          (TFun(X, t), reqs, subsol)
        case Some(treq) =>
          val otherReqs = reqs - x
          if (e.lits.size == 2) {
            val sol = subsol + EqConstraint(e.lits(1).asInstanceOf[Type], treq)
            (TFun(treq, t), otherReqs, sol)
          }
          else
            (TFun(treq, t), otherReqs, subsol)
      }
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, subsol) = e.kids(0).typ

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

      (tfun, restReqs, subsol)
    case If0 =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val (t3, reqs3, sol3) = e.kids(2).typ
      val subsol = sol1 ++++ sol2 ++++ sol3

      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      val sol = subsol ++ (cond +: body +: (mcons12 ++ mcons23))

      (t2, mreqs123, sol)

    case Fix =>
      val (t, reqs, subsol) = e.kids(0).typ
      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = subsol + fixCons
      (X, reqs, sol)
  }
}

object BottomUpSometimesEagerSubstCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpSometimesEagerSubstChecker(1000)
  def makeChecker(SUBST_THRESHOLD: Int) = new BottomUpSometimesEagerSubstChecker(SUBST_THRESHOLD)
}