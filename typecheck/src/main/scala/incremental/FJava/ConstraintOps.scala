package incremental.FJava

/**
 * Created by lirakuci on 3/16/15.
 */
import incremental.ConstraintOps._
import incremental.EqConstraint
import incremental.Type
import incremental.Type
import incremental.Type
import incremental.Type.Companion._
import incremental.Util
import incremental._

import incremental.Constraint
class ConstraintOps extends Serializable {
  incremental.ConstraintOps.constraintCount = 0
  incremental.ConstraintOps.constraintSolveTime = 0
  incremental.ConstraintOps.mergeSolutionTime = 0

  def constraintCount = incremental.ConstraintOps.constraintCount
  def constraintSolveTime = incremental.ConstraintOps.constraintSolveTime
  def mergeSolutionTime = incremental.ConstraintOps.mergeSolutionTime


  case class NotEqConstraint(expected: Type, actual: Type) extends Constraint {
    def solve(s: Solution) = expected.notUnify(actual)
    def finalize(s: Solution) = solve(s)
    def subst(s: TSubst) = EqConstraint(expected.subst(s), actual.subst(s))
  }

  private var _nextId = 0
  def freshUVar(): UCName = {
    val v = UCName(Symbol("x$" + _nextId))
    _nextId += 1
    v
  }


  var mergeReqsTime = 0.0
  var cmergeReqsTime = 0.0

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
}

