package incremental.pcf.with_subtyping


import incremental.Exp.Exp
import incremental.pcf._
import incremental.{Type => _, _}
import incremental.pcf.with_subtyping.Type.Companion
import TypeOps._

/**
 * Created by oliver on 20.11.14.
 */
class BottomUpChecker extends BUChecker[Type] {
  type CSystem = ConstraintOps.type
  val cs = ConstraintOps
  import cs._
  import localState.gen._

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Num =>
      (TNum, Map(), emptyCSet)
    case op if op == Add || op == Mul =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val sol = (sol1 ++ sol2 + Equal(TNum, t1) + Equal(TNum, t2) ++ mcons).trySolve
      (TNum, mreqs.mapValues(_.subst(sol.substitution)), sol)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> X), emptyCSet)
    case App =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val X = freshUVar(false)
      val Y = freshUVar()
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val sol = (sol1 ++ sol2 + Equal(t1, X -->: Y) + Subtype(t2, X) ++ mcons).trySolve
      (Y.subst(sol.substitution), mreqs.mapValues(_.subst(sol.substitution)), sol)
    case Abs if e.lits(0).isInstanceOf[Symbol] =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val annotatedT = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
      val (t, reqs, subsol) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          (TFun(annotatedT, t), reqs - x, subsol)
        case Some(treq) =>
          val otherReqs = reqs - x
          val sol = (subsol + Subtype(annotatedT, treq)).trySolve
          (TFun(annotatedT, t).subst(sol.substitution), otherReqs.mapValues(_.subst(sol.substitution)), sol)
      }
    case If0 =>
      val (t1, reqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, sol2) = e.kids(1).typ
      val (t3, reqs3, sol3) = e.kids(2).typ
      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)
      val Xjoin = freshUVar()
      val sol = (sol1 ++ sol2 ++ sol3 + Equal(TNum, t1) + Join(Xjoin, Set(t2, t3)) ++ mcons12 ++ mcons23).trySolve
      (Xjoin.subst(sol.substitution), mreqs123.mapValues(_.subst(sol.substitution)), sol)

    case Fix =>
      val (t, reqs, subsol) = e.kids(0).typ
      val X = freshUVar(false)
      val Y = freshUVar()
      val sol = (subsol + Equal(t, X -->: Y) + Subtype(Y, X)).trySolve
      (X.subst(sol.substitution), reqs.mapValues(_.subst(sol.substitution)), sol)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}