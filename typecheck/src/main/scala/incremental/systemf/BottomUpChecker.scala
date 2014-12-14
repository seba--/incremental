package incremental.systemf

import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Type.Companion._
import incremental.Exp._
import incremental._

/**
 * Created by seba on 13/11/14.
 */
class BottomUpChecker extends TypeChecker[Type] {

  type CSystem = ConstraintOps.type
  val cs = ConstraintOps
  import cs._
  import localState.gen._

  type TReqs = Set[Symbol]

  type Result = (Type, Requirements, TReqs, CSet)

  def typecheck(e: Exp): Either[Type, TError] = {
    cs.state.withValue(localState) {
      val root = e.withType[Result]
      //    val (uninitialized, ptime) = Util.timed {root.uninitialized}
      //    preparationTime += ptime

      val (res, ctime) = Util.timed {
        root.visitUninitialized { e =>
          e.typ = typecheckStep(e)
          true
        }

        val (t_, reqs, treqs, sol_) = root.typ
        val sol = sol_.tryFinalize
        val t = t_.subst(sol.substitution)

        if (!reqs.isEmpty)
          Right(s"Unresolved context requirements $reqs, treqs $treqs, type $t, unres ${sol.notyet}, unsat ${sol.never}")
        else if (!treqs.isEmpty)
          Right(s"Unresolved type variables requirements $treqs, type $t, unres ${sol.notyet}, unsat ${sol.never}")
        else if (!sol.isSolved)
          Right(s"Unresolved constraints ${sol.notyet}, unsat ${sol.never}, type $t")
        else
          Left(t)
      }
      localState.stats.typecheckTime += ctime
      res
    }
  }

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Num => (TNum, Map(),Set(), emptyCSet)
    case op if op == Add || op == Mul =>
      val (t1, reqs1, treqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, treqs2, sol2) = e.kids(1).typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val sol = emptyCSet ++ (lcons +: rcons +: mcons )
      (TNum, mreqs.mapValues(_.subst(sol.substitution)), treqs1 ++ treqs2, sol1 +++ sol2 <++ sol)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> X), Set(), emptyCSet)

    case App =>

      val (t1, reqs1, treqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, treqs2, sol2) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)


      val sol = emptyCSet ++ (fcons +: mcons)
      (X.subst(sol.substitution), mreqs.mapValues(_.subst(sol.substitution)), treqs1 ++ treqs2, sol1 +++ sol2 <++ sol)

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs, subsol) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
          (TFun(X, t), reqs, treqs ++ X.freeTVars, subsol)
        case Some(treq) =>
          val otherReqs = reqs - x
          if (e.lits.size == 2) {
            val X = e.lits(1).asInstanceOf[Type]
            val sol = emptyCSet + EqConstraint(X, treq)
            (TFun(treq, t).subst(sol.substitution), otherReqs.mapValues(_.subst(sol.substitution)), treqs ++ X.freeTVars, subsol <++ sol)
          }
          else
            (TFun(treq, t), otherReqs, treqs, subsol)
      }
    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, treqs, subsol) = e.kids(0).typ

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

      (tfun, restReqs, treqs, subsol)
    case If0 =>
      val (t1, reqs1, treqs1, sol1) = e.kids(0).typ
      val (t2, reqs2, treqs2, sol2) = e.kids(1).typ
      val (t3, reqs3, treqs3, sol3) = e.kids(2).typ

      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      val sol = emptyCSet ++ (cond +: body +: (mcons12 ++ mcons23))

      (t2.subst(sol.substitution), mreqs123.mapValues(_.subst(sol.substitution)), treqs1 ++ treqs2 ++ treqs3, sol1 +++ sol2 +++ sol3 <++ sol)

    case Fix =>
      val (t, reqs, treqs, subsol) = e.kids(0).typ
      val X = freshUVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val sol = emptyCSet + fixCons
      (X.subst(sol.substitution), reqs.mapValues(_.subst(sol.substitution)), treqs, subsol <++ sol)

    case TAbs if (e.lits(0).isInstanceOf[Symbol]) =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs, subsol) = e.kids(0).typ
      (TUniv(alpha, t), reqs, treqs-alpha, subsol)

    case TApp =>
      val (t1, reqs1, treqs, subsol) = e.kids(0).typ
      val t = e.lits(0).asInstanceOf[Type]

      val Xalpha = freshUVar().x
      val Xbody = freshUVar()
      val Xres = freshUVar()

      val ucons = EqConstraint(UUniv(Xalpha, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      val sol = emptyCSet ++ Seq(ucons, vcons)

      (Xres.subst(sol.substitution), reqs1.mapValues(_.subst(sol.substitution)), treqs ++ t.freeTVars, subsol <++ sol)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}