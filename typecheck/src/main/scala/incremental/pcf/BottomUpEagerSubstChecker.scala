package incremental.pcf

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Type.Companion._
import incremental.Exp._
import incremental._

/**
 * Created by seba on 13/11/14.
 */
class BottomUpEagerSubstChecker extends BUChecker[Type] {
  type CSystem = ConstraintOps.type
  val cs = ConstraintOps
  import cs._
  import localState.gen._

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Num => (TNum, Map(), emptyCSet)
    case op if op == Add || op == Mul =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = emptyCSet ++ Seq(lcons, rcons) ++ mcons
      (TNum, mreqs.mapValues(_.subst(sol.substitution)), sol1 +++ sol2 <++ sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> X), emptyCSet)
    case App =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = emptyCSet ++ (fcons +: mcons)
      (X.subst(sol.substitution), mreqs.mapValues(_.subst(sol.substitution)), sol1 +++ sol2 <++ sol)
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
            val sol = emptyCSet + EqConstraint(e.lits(1).asInstanceOf[Type], treq)
            (TFun(treq, t).subst(sol.substitution), otherReqs.mapValues(_.subst(sol.substitution)), subsol <++ sol)
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

      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      val sol = emptyCSet ++ (cond +: body +: (mcons12 ++ mcons23))

      (t2.subst(sol.substitution), mreqs123.mapValues(_.subst(sol.substitution)), sol1 +++ sol2 +++ sol3 <++ sol)

    case Fix =>
      val (t, reqs, subsol) = e.kids(0).typ
      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = emptyCSet + fixCons
      (X.subst(sol.substitution), reqs.mapValues(_.subst(sol.substitution)), subsol <++ sol)
  }
}

object BottomUpEagerSubstCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpEagerSubstChecker
}