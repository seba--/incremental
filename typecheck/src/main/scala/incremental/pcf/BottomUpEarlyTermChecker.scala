package incremental.pcf

import incremental.{ConstraintOps, EqConstraint, TypeCheckerFactory, Exp_}

/**
 * Created by seba on 14/11/14.
 */
class BottomUpEarlyTermChecker extends BottomUpChecker {

  override def typecheckSpine(e: Exp_[Result]): Unit ={
    var current = e
    while (current != null && current.allKidTypesAvailable) {
      val isFirstTime = !current.valid
      val isRoot = current.parent == null

      val t = typecheckStep(current)
      //      println(s"$current -> t")
      //      println(s"  old: ${current.typ}")

      if (!isFirstTime && sameResult(current.typ, t))
        return

      current.typ = t
      if (!isRoot && isFirstTime)
        current.parent.markKidTypeAvailable(current.pos)
      current = current.parent
    }
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

    if (!sol.isSolved)
      return false

    val s = sol.solution
    val notyetEquiv = sol1.never.zip(sol2.never).foldLeft(true)((b,p) => b && p._1.subst(s) == p._2.subst(s))
    val neverEquiv = sol1.never.zip(sol2.never).foldLeft(true)((b,p) => b && p._1.subst(s) == p._2.subst(s))
    notyetEquiv && neverEquiv
  }
}

object BottomUpEarlyTermCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new BottomUpEarlyTermChecker
}
