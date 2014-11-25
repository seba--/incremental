package incremental.systemf

import incremental.Type.TSubst
import incremental.{Util, Type}

/**
 * Created by seba on 13/11/14.
 */
class Constraint {
  type Unsolvable = Seq[EqConstraint]
  type Solution = (TSubst, Unsolvable)
  val emptySol: Solution = (Map(), Seq())

  private var _nextId = 0
  def freshTVar(): TVarInternal = {
    val v = TVarInternal(Symbol("x$" + _nextId))
    _nextId += 1
    v
  }

  var constraintCount = 0
  var mergeReqsTime = 0.0
  var constraintSolveTime = 0.0
  var mergeSolutionTime = 0.0

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


  def solve(cs: Iterable[EqConstraint], sol: Solution = emptySol): Solution = {
    constraintCount += cs.size
    val (res, time) = Util.timed(cs.foldLeft(sol)(extendSolution))
    constraintSolveTime += time
    res
  }
  def _solve(cs: Iterable[EqConstraint], sol: Solution): Solution = {
    cs.foldLeft(sol)(extendSolution)
  }
  def solve(c: EqConstraint): Solution = {
    constraintCount += 1
    val (res, time) = Util.timed(extendSolution(emptySol, c))
    constraintSolveTime += time
    res
  }
  def solve(c: EqConstraint, sol: Solution): Solution = {
    constraintCount += 1
    val (res, time) = Util.timed(extendSolution(sol, c))
    constraintSolveTime += time
    res
  }

  private def extendSolution(sol: Solution, c: EqConstraint): Solution = {
    c.solve(sol._1) match {
      case None => (sol._1, c +: sol._2)
      case Some(u) =>
        var s = sol._1.mapValues(_.subst(u))
        var unres = sol._2
        for ((x, t2) <- u) {
          s.get(x) match {
            case None => s += x -> t2.subst(s)
            case Some(t1) => t1.unify(t2, s) match {
              case None => unres = EqConstraint(t1, t2) +: unres
              case Some(u) => s = s.mapValues(_.subst(u)) ++ u
            }
          }
        }
        (s, unres)
    }
  }

  def mergeSolution(sol1: Solution, sol2: Solution): Solution = {
    val (res, time) = Util.timed(_mergeSolution(sol1, sol2))
    mergeSolutionTime += time
    res
  }

  private def _mergeSolution(sol1: Solution, sol2: Solution): Solution = {
    val s1 = sol1._1
    val s2 = sol2._1
    var unres: Unsolvable = sol1._2 ++ sol2._2

    var s = s1 mapValues (_.subst(s2))

    for ((x, t2) <- s2) {
      s.get(x) match {
        case None => s += x -> t2.subst(s)
        case Some(t1) => t1.unify(t2, s) match {
          case None => unres = EqConstraint(t1, t2) +: unres
          case Some(u) => s = s.mapValues(_.subst(u)) ++ u
        }
      }
    }

    (s, unres)
  }
}

case class EqConstraint(expected: Type, actual: Type) {
  def solve(s: TSubst) = expected.unify(actual, s)
}
