package incremental.pcf

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

  private var _nextId = 0
  def freshTVar(): TVar = {
    val v = TVar(Symbol("x$" + _nextId))
    _nextId += 1
    v
  }

  def typecheck(e: Exp): Either[Type, TError] = {
    _nextId = 0
    val root = e.withType[Result]

    val (leaves, ptime) = Util.timed {root.leaves}
    preparationTime += ptime

    val (res, ctime) = Util.timed {
      leaves foreach (typecheckSpine(_))

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
      val isFirstTime = current.typ == null
      val isRoot = current.parent == null

      val t = typecheckStep(current)
//            println(s"$current -> ")
//            println(s" * ${t._1}")
//            println(s" * ${t._2}")
//            println(s" * ${t._3}")

      if (!isFirstTime && current.typ == t)
        return

      current.typ = t
      if (!isRoot && isFirstTime)
        current.parent.markKidTypeAvailable(current.pos)
      current = current.parent
    }
  }

  def typecheckStep(e: Exp_[Result]): Result = e.kind match {
    case Num => (TNum, Map(), Set())
    case op if op == Add || op == Mul =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ

      val lcons = EqConstraint(TNum, t1)
      val rcons = EqConstraint(TNum, t2)

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val (s, newunres) = solve(mcons + lcons + rcons)
      (TNum, mreqs.mapValues(_.subst(s)), unres1 ++ unres2 ++ newunres)
    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshTVar()
      (X, Map(x -> X), Set())
    case App =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ

      val X = freshTVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

      val (s, newunres) = solve(mcons + fcons)

      (X.subst(s), mreqs.mapValues(_.subst(s)), unres1 ++ unres2 ++ newunres)
    case Abs =>
      if (e.lits(0).isInstanceOf[Symbol]) {
        val x = e.lits(0).asInstanceOf[Symbol]
        val (t, reqs, unres) = e.kids(0).typ

        reqs.get(x) match {
          case None =>
            val X = freshTVar()
            (TFun(X, t), reqs, unres)
          case Some(treq) =>
            val otherReqs = reqs - x
            (TFun(treq, t), otherReqs, unres)
        }
      }
      else if (e.lits(0).isInstanceOf[Seq[Symbol]]) {
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
      }
      else
        throw new RuntimeException(s"Cannot handle Abs variables ${e.lits(0)}")
    case If0 =>
      val (t1, reqs1, unres1) = e.kids(0).typ
      val (t2, reqs2, unres2) = e.kids(1).typ
      val (t3, reqs3, unres3) = e.kids(2).typ

      val (mcons12, mreqs12) = mergeReqMaps(reqs1, reqs2)
      val (mcons23, mreqs123) = mergeReqMaps(mreqs12, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      val (s, newunres) = solve(mcons12 ++ mcons23 + cond + body)

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