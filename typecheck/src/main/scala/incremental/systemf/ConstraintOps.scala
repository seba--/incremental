package incremental.systemf

import incremental.ConstraintOps._
import incremental.Type.Companion._
import incremental._


class ConstraintOps {
  incremental.ConstraintOps.constraintCount = 0
  incremental.ConstraintOps.constraintSolveTime = 0
  incremental.ConstraintOps.mergeSolutionTime = 0

  def constraintCount = incremental.ConstraintOps.constraintCount
  def constraintSolveTime = incremental.ConstraintOps.constraintSolveTime
  def mergeSolutionTime = incremental.ConstraintOps.mergeSolutionTime

  private var _nextId = 0
  def freshTVar(): TVarInternal = {
    val v = TVarInternal(Symbol("x$" + _nextId))
    _nextId += 1
    v
  }

  var mergeReqsTime = 0.0

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

  var tmergeReqsTime = 0.0

  def tmergeReqMaps(reqs1: Map[Symbol, Type], reqs2: Map[Symbol, Type]) = {
    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
    tmergeReqsTime += time
    res
  }

  def _tmergeReqMaps(reqs1: Map[Symbol, Type], reqs2: Map[Symbol, Type]) = {
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

  case class EqSubstConstraint(body: Type, alpha: Symbol, alphaIsInternal: Boolean, substitute: Type, result: Type) extends Constraint {
    private def withResult(t: Type, s: Solution) = t.unify(result, s.solution)

    private def substAlpha(s: TSubst) = if (!alphaIsInternal) (alpha, false) else s.get(alpha) match {
      case Some(TVar(beta)) => (beta, false)
      case Some(TVarInternal(beta)) => (beta, true)
      case None => (alpha, alphaIsInternal)
      case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
    }

    def solve(s: Solution) = {
      val tbody = body.subst(s.solution)
      val (beta, betaIsInternal) = substAlpha(s.solution)

      tbody match {
        case TVar(`beta`) | TVarInternal(`beta`) => withResult(substitute, s)
        case TVar(_) if !betaIsInternal => withResult(tbody, s) // because alpha is user-defined and different

        case TNum => withResult(TNum, s)
        case TFun(t1, t2) =>
          val X = freshTVar()
          val Y = freshTVar()
          (EqSubstConstraint(t1, beta, betaIsInternal, substitute, X).solve(s) ++
           EqSubstConstraint(t2, beta, betaIsInternal, substitute, Y).solve(s) ++
           withResult(TFun(X, Y), s))
        case TUniv(`beta`, _) => withResult(tbody, s)
        case TUniv(gamma, t) if !betaIsInternal =>
          val X = freshTVar()
          (EqSubstConstraint(t, beta, betaIsInternal, substitute, X).solve(s) ++
           withResult(TUniv(gamma, X), s))

        // either:
        // - tbody == TVarInternal   <== Example: x. (x [Num]) + (x [Num -> Num])
        // - tbody == TVar && betaIsInternal ==> cannot happen, type argument always has to become concrete
        // - tbody == TUnivInternal ==> cannot happen, universal types have explicit argument in TAbs('alpha, t)
        // - tbody == TUni && betaIsInternal ==> cannot happen, type argument always has to become concrete
        case _ => notyet(EqSubstConstraint(tbody, beta, betaIsInternal, substitute, result))
      }
    }

    def finalize(s: Solution) = solve(s)
//    {
//      val trec = record.subst(s.solution)
//      trec match {
//        case TRecord(fields) =>
//          fields.get(label) match {
//            case None => never(EqRecordProjectConstraint(trec, label, field))
//            case Some(t) => EqConstraint(t, field).solve(s)
//          }
//        case TVar(x) =>
//          var cons = Seq[Constraint]()
//          var fields = Map(label -> field.subst(s.solution))
//          for (EqRecordProjectConstraint(TVar(`x`), l, field) <- s.notyet)
//            if (!fields.isDefinedAt(l))
//              fields += l -> field.subst(s.solution)
//            else
//              cons = EqConstraint(fields(l), field) +: cons
//          solution(Map(x -> TRecord(fields))) ++ ConstraintOps.solve(cons, s)
//        case _ => never(EqRecordProjectConstraint(trec, label, field))
//      }
//    }

    def subst(s: TSubst) = {
      val (newalpha, newalphaIsInternal) = substAlpha(s)
      EqSubstConstraint(body.subst(s), newalpha, newalphaIsInternal, substitute.subst(s), result.subst(s))
    }
  }

}