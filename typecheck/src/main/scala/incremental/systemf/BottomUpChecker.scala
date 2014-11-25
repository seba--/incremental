package incremental.systemf

import incremental.Exp.Exp
import incremental.Type._
import incremental.Exp._
import incremental._

/**
 * Created by seba on 13/11/14.
 */
class BottomUpChecker extends TypeChecker {

  val constraint = new Constraint
  import constraint._

  var preparationTime = 0.0
  var typecheckTime = 0.0
  def constraintCount = constraint.constraintCount
  def mergeReqsTime = constraint.mergeReqsTime
  def constraintSolveTime = constraint.constraintSolveTime

  type Result = (Type, TSubst, Unsolvable)

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    preparationTime += ptime

    val (res, ctime) = Util.timed {
      uninitialized foreach (e => if (!e.valid) typecheckSpine(e))

      val (t, reqs, unres) = root.typ
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

  def typecheckSpine(e: Exp_[Result]): Unit ={
    var current = e
    while (current != null && current.allKidTypesAvailable) {
      val isFirstTime = !current.valid
      val isRoot = current.parent == null

      val t = typecheckStep(current)
//      println(s"$current -> $t")
//      println(s"  old: ${current.typ}")

      current.typ = t
      if (!isRoot && isFirstTime)
        current.parent.markKidTypeAvailable(current.pos)
      current = current.parent
    }
  }

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {

    case Num => (TNum, Map(), Seq())
    case op if op == Add || op == Mul =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val (s, newunres) = solve(lcons +: rcons +: mcons)
      (TNum, mreqs.mapValues(_.subst(s)), unres1 ++ unres2 ++ newunres)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshTVar()
      (X, Map(x -> X), Seq())

    case TApp =>

      val (t1, reqs1, unres1) = e.kids(0).typ
      val t = e.lits(0).asInstanceOf[Type]

      val Xalpha = freshTVar().x
      val Xbody = freshTVar()
      val Xres = freshTVar()

      (Xalpha, Map(Xalpha -> t), Seq())

     // val vcons = EqConstraint(Xbody.subst(Xalpha -> t), Xres)
      val ucons = EqConstraint(TUnivInternal(Xalpha, Xbody), t1)
      val (s, newunres) = solve(Seq( ucons))// , vcons

      (Xres.subst(s), reqs1.mapValues(_.subst(s)), unres1 ++  newunres)

    case App =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ

      val X = freshTVar()

      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val (s, newunres) = solve(fcons +: mcons)

      (X.subst(s), mreqs.mapValues(_.subst(s)), unres1 ++ unres2 ++ newunres)

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, unres) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshTVar()
          (TFun(X, t), reqs, unres)
        case Some(treq) =>
          val otherReqs = reqs - x
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else treq
          val (s, newunres)  = solve(EqConstraint(X, treq))
          (TFun(treq, t).subst(s), otherReqs.mapValues(_.subst(s)), unres ++ newunres)
      }

    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
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

      (tfun, restReqs, unres)

    case TAbs if (e.lits(0).isInstanceOf[Symbol]) =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, unres) = e.kids(0).typ
      (TUniv(alpha, t), reqs - alpha, unres)

    case If0 =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ
      val (t3, reqs3, unres3) = e.kids(2).typ

      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      val (s, newunres) = solve(cond +: body +: (mcons12 ++ mcons23))

      (t2.subst(s), mreqs123.mapValues(_.subst(s)), unres1 ++ unres2 ++ unres3 ++ newunres)

    case Fix =>
      val (t, reqs, unres) = e.kids(0).typ
      val X = freshTVar()
      val fixCons = EqConstraint(t, TFun(X, X))
      val (s, newunres) = solve(fixCons)
      (X.subst(s), reqs.mapValues(_.subst(s)), unres ++ newunres)
  }
}

object BottomUpCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new BottomUpChecker
}