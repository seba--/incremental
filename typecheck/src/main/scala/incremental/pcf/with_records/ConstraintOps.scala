package incremental.pcf.with_records

import incremental.ConstraintOps._
import incremental.Type.Companion.TSubst
import incremental._
import incremental.pcf.TVar

/**
 * Created by seba on 13/11/14.
 */
class ConstraintOps extends incremental.pcf.ConstraintOps {
//  def mergeReqMaps(reqs1: Map[Symbol, Type], reqs2: Map[Symbol, Type]) = {
//    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
//    mergeReqsTime += time
//    res
//  }
//
//  def _mergeReqMaps(reqs1: Map[Symbol, Type], reqs2: Map[Symbol, Type]) = {
//    var mcons = Seq[EqConstraint]()
//    var mreqs = reqs1
//    for ((x, r2) <- reqs2)
//      reqs1.get(x) match {
//        case None => mreqs += x -> r2
//        case Some(r1) =>
//          mcons = EqConstraint(r1, r2) +: mcons
//      }
//
//    (mcons, mreqs)
//  }
//
//
//  def solve(cs: Iterable[EqConstraint], sol: Solution = emptySol): Solution = {
//    constraintCount += cs.size
//    val (res, time) = Util.timed(cs.foldLeft(sol)(extendSolution))
//    constraintSolveTime += time
//    res
//  }
//  def _solve(cs: Iterable[EqConstraint], sol: Solution): Solution = {
//    cs.foldLeft(sol)(extendSolution)
//  }
//  def solve(c: EqConstraint): Solution = {
//    constraintCount += 1
//    val (res, time) = Util.timed(extendSolution(emptySol, c))
//    constraintSolveTime += time
//    res
//  }
//  def solve(c: EqConstraint, sol: Solution): Solution = {
//    constraintCount += 1
//    val (res, time) = Util.timed(extendSolution(sol, c))
//    constraintSolveTime += time
//    res
//  }
//
//  private def extendSolution(sol: Solution, c: EqConstraint): Solution = {
//    c.solve(sol._1) match {
//      case None => (sol._1, c +: sol._2)
//      case Some(u) =>
//        var s = sol._1.mapValues(_.subst(u))
//        var unres = sol._2
//        for ((x, t2) <- u) {
//          s.get(x) match {
//            case None => s += x -> t2.subst(s)
//            case Some(t1) => t1.unify(t2, s) match {
//              case None => unres = EqConstraint(t1, t2) +: unres
//              case Some(u) => s = s.mapValues(_.subst(u)) ++ u
//            }
//          }
//        }
//        (s, unres)
//    }
//  }
//
//  def mergeSolution(sol1: Solution, sol2: Solution): Solution = {
//    val (res, time) = Util.timed(_mergeSolution(sol1, sol2))
//    mergeSolutionTime += time
//    res
//  }
//
//  private def _mergeSolution(sol1: Solution, sol2: Solution): Solution = {
//    val s1 = sol1._1
//    val s2 = sol2._1
//    var unres: Unsolvable = sol1._2 ++ sol2._2
//
//    var s = s1 mapValues (_.subst(s2))
//
//    for ((x, t2) <- s2) {
//      s.get(x) match {
//        case None => s += x -> t2.subst(s)
//        case Some(t1) => t1.unify(t2, s) match {
//          case None => unres = EqConstraint(t1, t2) +: unres
//          case Some(u) => s = s.mapValues(_.subst(u)) ++ u
//        }
//      }
//    }
//
//    (s, unres)
//  }
}

case class EqRecordProjectConstraint(record: Type, label: Symbol, field: Type) extends Constraint {
  def solve(s: Solution) = {
    val trec = record.subst(s.solution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(s)
        }
      case TVar(_) => notyet(EqRecordProjectConstraint(trec, label, field))
      case _ => never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def finalize(s: Solution) = {
    val trec = record.subst(s.solution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(s)
        }
      case TVar(x) =>
        var cons = Seq[Constraint]()
        var fields = Map(label -> field.subst(s.solution))
        for (EqRecordProjectConstraint(TVar(`x`), l, field) <- s.notyet)
          if (!fields.isDefinedAt(l))
            fields += l -> field.subst(s.solution)
          else
            cons = EqConstraint(fields(l), field) +: cons
        solution(Map(x -> TRecord(fields))) ++ ConstraintOps.solve(cons, s)
      case _ => never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def subst(s: TSubst) = EqRecordProjectConstraint(record.subst(s), label, field.subst(s))
}
