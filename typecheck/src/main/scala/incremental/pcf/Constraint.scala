package incremental.pcf

import incremental.Type.TSubst
import incremental.{Util, Type}

/**
 * Created by seba on 13/11/14.
 */
class Constraint {
  type Unsolvable = Set[EqConstraint]
  type Solution = (TSubst, Unsolvable)
  val emptySol: Solution = (Map(), Set())

  var constraintCount = 0
  var mergeReqsTime = 0.0
  var constraintSolveTime = 0.0

  def mergeReqMaps(reqs1: Map[Symbol, Type], reqs2: Map[Symbol, Type]) = {
    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
    mergeReqsTime += time
    res
  }

  def _mergeReqMaps(reqs1: Map[Symbol, Type], reqs2: Map[Symbol, Type]) = {
    var mcons = Seq[EqConstraint]()
    var mreqs = reqs1
    for ((x, r2) <- reqs2)
      reqs1.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = EqConstraint(r1, r2) +: mcons
      }

    (mcons, mreqs)
  }


  def solve(cs: Iterable[EqConstraint]): Solution = {
    constraintCount += cs.size
    val (res, time) = Util.timed(cs.foldLeft(emptySol)(extendSolution))
    constraintSolveTime += time
    res
  }
  def solve(c: EqConstraint): Solution = {
    constraintCount += 1
    val (res, time) = Util.timed(c.solve(Map()) match {
        case None => emptySol
        case Some(s) => (s, Set[EqConstraint]())
      }
    )
    constraintSolveTime += time
    res
  }

  private def extendSolution(sol: Solution, c: EqConstraint): Solution = {
    c.solve(sol._1) match {
      case None => (sol._1, sol._2 + c)
      case Some(u) =>
        var s = sol._1.mapValues(_.subst(u))
        var unres = sol._2
        for ((x, t2) <- u) {
          s.get(x) match {
            case None => s += x -> t2.subst(s)
            case Some(t1) => t1.unify(t2, s) match {
              case None => unres += EqConstraint(t1, t2)
              case Some(u) => s = s.mapValues(_.subst(u)) ++ u
            }
          }
        }
        (s, unres)
    }
  }
}

case class EqConstraint(expected: Type, actual: Type) {
  def solve(s: TSubst) = expected.unify(actual, s)
}
