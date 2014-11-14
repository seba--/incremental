package incremental.pcf

import incremental.{TypeCheckerFactory, Exp_}

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
    val (t1, reqs1, unres1) = r1
    val (t2, reqs2, unres2) = r2

    if (unres1.size != unres2.size)
      return false

    val (mcons, _) = constraint.mergeReqMaps(reqs1, reqs2)
    val (s, unres) = constraint.solve(EqConstraint(t1, t2) +: mcons)

    if (!unres.isEmpty)
      return false

    for (i <- 0 until unres1.size) {
      val EqConstraint(t11, t12) = unres1(i)
      val EqConstraint(t21, t22) = unres2(i)
      if (t11.subst(s) != t21.subst(s) || t12.subst(s) != t22.subst(s))
        return false
    }
    true
  }
}

object BottomUpEarlyTermCheckerFactory extends TypeCheckerFactory {
  def makeChecker = new BottomUpEarlyTermChecker
}
