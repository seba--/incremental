package incremental.pcf

import incremental.Exp._
import incremental._

/**
 * Created by seba on 14/11/14.
 */
class BottomUpEarlyTermChecker extends BottomUpChecker {

  override def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    //    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    //    preparationTime += ptime

    val (res, ctime) = Util.timed {
      root.visitUninitialized {e =>
        val t = typecheckStep(e)
        if (e.typ != null && sameResult(e.typ, t)) {
          e.typ = t
          false
        }
        else {
          e.typ = t
          true
        }
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
    typecheckTime += ctime
    res
  }

  def sameResult(r1: Result, r2: Result): Boolean = {
    val (t1, reqs1, sol1_) = r1
    val (t2, reqs2, sol2_) = r2
    val sol1 = sol1_.trySolveNow
    val sol2 = sol2_.trySolveNow

    if (sol1.never.size != sol2.never.size || sol1.notyet.size != sol2.notyet.size)
      return false

    val (mcons, _) = constraint.mergeReqMaps(reqs1, reqs2)
    val sol = ConstraintOps.solve(EqConstraint(t1, t2) +: mcons).trySolveNow

    val s = sol.substitution
    val isRenaming = s.foldLeft(true)((b, p) => b && p._2.isInstanceOf[UVar])
    if (!sol.isSolved || !isRenaming)
      return false

    val notyetEquiv = sol1.never.zip(sol2.never).foldLeft(true)((b,p) => b && p._1.subst(s) == p._2.subst(s))
    val neverEquiv = sol1.never.zip(sol2.never).foldLeft(true)((b,p) => b && p._1.subst(s) == p._2.subst(s))
    notyetEquiv && neverEquiv
  }
}

object BottomUpEarlyTermCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpEarlyTermChecker
}
