package incremental.FJava

/**
 * Created by lirakuci on 3/16/15.
 */
import incremental.ConstraintOps._
import incremental.EqConstraint
import incremental.Type
import incremental.Type.Companion._
import incremental.Util
import incremental.Constraint
import incremental.pcf.UVar

class ConstraintOps extends Serializable {
  incremental.ConstraintOps.constraintCount = 0
  incremental.ConstraintOps.constraintSolveTime = 0
  incremental.ConstraintOps.mergeSolutionTime = 0

  def constraintCount = incremental.ConstraintOps.constraintCount
  def constraintSolveTime = incremental.ConstraintOps.constraintSolveTime
  def mergeSolutionTime = incremental.ConstraintOps.mergeSolutionTime


  case class EqConstraintC(t1: Type, cld1: ClassDecl, t2: Type, cld2: ClassDecl) extends Constraint {
    def solve(s: Solution) =t1.unify(t2, s.substitution)
    def finalize(s: Solution) = solve(s)
    def subst(s: TSubst) = EqConstraint(t1.subst(s), t2.subst(s))
  }

  case class NotEqConstraint(expected: Type, actual: Type) extends Constraint {
    def solve(s: Solution) = expected.notUnify(actual)
    def finalize(s: Solution) = solve(s)
    def subst(s: TSubst) = EqConstraint(expected.subst(s), actual.subst(s))
  }

  private var _nextId = 0
  def freshUVar(): UVar = {
    val v = UVar(Symbol("x$" + _nextId))
    _nextId += 1
    v
  }

  private var _nextIdC = 0
  def freshCName() : UCName = {
    val v = UCName(Symbol("x$" + _nextIdC))
    _nextIdC +=1
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

  def mergeCld(cld1: ClassDecl, cld2 : ClassDecl) : ClassDecl = {
return cld1

  }

  def mergeCReqMaps(creqs1: Map[Type, ClassDecl], creqs2: Map[Type, ClassDecl]) = {
    val (cres, time) = Util.timed(_mergeCReqMaps(creqs1, creqs2))
  }


  def _mergeCReqMaps(creqs1: Map[Type, ClassDecl], creqs2: Map[Type, ClassDecl]) = {
    var mcons = Seq[EqConstraint]()
    var mreqs = creqs1
    for ((t, cld2) <- creqs2)
      creqs1.filterKeys { t => true } // match {
    //case None => mreqs += t -> cld2
    // case (t1, cld1) :: lt =>
    //'mcons = EqConstraint(t, t1) +: mcons

 // }

    (mcons, mreqs)
  }
}