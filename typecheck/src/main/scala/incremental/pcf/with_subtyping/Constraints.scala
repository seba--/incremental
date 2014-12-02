package incremental.pcf.with_subtyping

import incremental.ConstraintOps._
import incremental.Type._
import incremental._
import incremental.pcf.{ConstraintOps => _, TFun, TNum, TVar}
import TypeOps._

class ConstraintOps {
  incremental.ConstraintOps.constraintCount = 0
  incremental.ConstraintOps.constraintSolveTime = 0
  incremental.ConstraintOps.mergeSolutionTime = 0

  def constraintCount = incremental.ConstraintOps.constraintCount
  def constraintSolveTime = incremental.ConstraintOps.constraintSolveTime
  def mergeSolutionTime = incremental.ConstraintOps.mergeSolutionTime

  private var _nextId = 0
  def freshTVar(): TVar = {
    val v = TVar(Symbol("x$" + _nextId))
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
    var mcons = Seq[Constraint]()
    var mreqs = reqs1
    for ((x, r2) <- reqs2)
      reqs1.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = EqConstraint(r1, r2) +: mcons
          mreqs += x -> r1
      }

    (mcons, mreqs)
  }


  case class EqMeetConstraint(t1: Type, t2: Type, tmeet: Type) extends Constraint {
    private def withMeet(t: Type, s: Solution) = t.unify(tmeet, s.solution)

    def solve(s: Solution): Solution = (t1, t2) match {
      case _ if t1 == t2 => withMeet(t1, s)
      case (_, Top) => withMeet(t1, s)
      case (Top, _) => withMeet(t2, s)
      case (Bot, _) | (_, Bot) => withMeet(Bot, s)
      case (TVar(x), TVar(y)) if x == y => withMeet(t1, s)
      case (TVar(a), _) =>
        s.solution.get(a) match {
          case Some(t1) => EqMeetConstraint(t1, t2, tmeet).solve(s)
          case None => notyet(this)
        }
      case (_, TVar(a)) =>
        s.solution.get(a) match {
          case Some(t2) => EqMeetConstraint(t1, t2, tmeet).solve(s)
          case None => notyet(this)
        }
      case (TFun(s1, t1), TFun(s2, t2)) =>
        val X = freshTVar()
        val Y = freshTVar()
        EqJoinConstraint(s1, s2, X).solve(s) ++ EqMeetConstraint(t1, t2, Y).solve(s) ++ withMeet(TFun(X, Y), s)
      case _ => never(this) //withMeet(Bot, s)
    }

    def finalize(s: Solution) = {
      val sol = solve(s)
      if (sol.notyet.size == 1 && sol.notyet(0) == this)
        (t1, t2) match {
          case (TVar(a), _) => SubConstraint(t2, t1).solve(s).tryFinalize ++ withMeet(t2, s)
          case (_, TVar(a)) => SubConstraint(t1, t2).solve(s).tryFinalize ++ withMeet(t1, s)
        }
      else
        sol.tryFinalize
    }

    def subst(s: TSubst) = EqMeetConstraint(t1.subst(s), t2.subst(s), tmeet.subst(s))
  }

  case class EqJoinConstraint(t1: Type, t2: Type, tjoin: Type) extends Constraint {
    private def withJoin(t: Type, s: Solution) = t.unify(tjoin, s.solution)

    def solve(s: Solution) = (t1, t2) match {
      case _ if t1 == t2 => withJoin(t1, s)
      case (_, Top) | (Top, _) => withJoin(Top, s)
      case (Bot, _) => withJoin(t2, s)
      case (_, Bot) => withJoin(t1, s)
      case (TVar(a), _) =>
        s.solution.get(a) match {
          case Some(t1) => EqJoinConstraint(t1, t2, tjoin).solve(s)
          case None => notyet(this)
        }
      case (_, TVar(a)) =>
        s.solution.get(a) match {
          case Some(t2) => EqJoinConstraint(t1, t2, tjoin).solve(s)
          case None => notyet(this)
        }
      case (TFun(s1, t1), TFun(s2, t2)) =>
        val X = freshTVar()
        val Y = freshTVar()
        EqMeetConstraint(s1, s2, X).solve(s) ++ EqJoinConstraint(t1, t2, Y).solve(s) ++ withJoin(TFun(X, Y), s)
      case _ => withJoin(Top, s)
    }

    def finalize(s: Solution) = {
      val sol = solve(s)
      if (sol.notyet.size == 1 && sol.notyet(0) == this)
        (t1, t2) match {
          case (TVar(a), _) => SubConstraint(t1, t2).solve(s).tryFinalize ++ withJoin(t2, s)
          case (_, TVar(a)) => SubConstraint(t2, t1).solve(s).tryFinalize ++ withJoin(t1, s)
        }
      else
        sol.tryFinalize
    }

    def subst(s: TSubst) = EqJoinConstraint(t1.subst(s), t2.subst(s), tjoin.subst(s))
  }

  case class SubConstraint(lower: Type, upper: Type) extends Constraint {
    def solve(s: Solution) = (lower,upper) match {
      case _ if s.notyet.contains(this) => emptySol
      case (t1, t2) if t1 == t2 => emptySol
      case (_, Top) => emptySol
      case (Bot, _) => emptySol
      case (TVar(a), t2) =>
        s.solution.get(a) match {
          case Some(t1) => SubConstraint(t1, t2).solve(s)
          case None =>
            if (t2.occurs(a))
              never(SubConstraint(lower, upper.subst(s.solution)))
            else
              notyet(SubConstraint(lower, upper.subst(s.solution)))
        }
      case (t1, TVar(a)) =>
        s.solution.get(a) match {
          case Some(t2) => SubConstraint(t1, t2).solve(s)
          case None =>
            if (t1.occurs(a))
              never(SubConstraint(lower.subst(s.solution), upper))
            else
              notyet(SubConstraint(lower.subst(s.solution), upper))
        }
      case (TFun(s1, t1), TFun(s2, t2)) =>
        SubConstraint(s2, s1).solve(s) ++ SubConstraint(t1, t2).solve(s)
      case _ => never(this)
    }

    def finalize(s: Solution) = {
      val sol = ConstraintOps.solve(this +: s.notyet, solution(s.solution))

      var doneLower = Set[Symbol]()
      var doneUpper = Set[Symbol]()
      var cons = Seq[Constraint]()
      var notyet = Seq[Constraint]()
      for (c <- sol.notyet) c match {
        case SubConstraint(t1, TVar(a)) =>
          if (!doneLower.contains(a)) {
            doneLower += a
            val bounds = sol.notyet.flatMap {
              case SubConstraint(t, TVar(`a`)) => Some(t)
              case EqMeetConstraint(TVar(`a`), _, tmeet) => Some(tmeet)
              case EqMeetConstraint(_, TVar(`a`), tmeet) => Some(tmeet)
              case _ => None
            }
            cons = cons ++ lowerBound(bounds, TVar(a))
          }
        case SubConstraint(TVar(a), t1) =>
          if (!doneUpper.contains(a)) {
            doneUpper += a
            val bounds = sol.notyet.flatMap {
              case SubConstraint(TVar(`a`), t) => Some(t)
              case EqJoinConstraint(TVar(`a`), _, tjoin) => Some(tjoin)
              case EqJoinConstraint(_, TVar(`a`), tjoin) => Some(tjoin)
              case _ => None
            }
            cons = cons ++ upperBound(bounds, TVar(a))
          }
        case _ => notyet = c +: notyet
      }

      ConstraintOps.solve(cons, Solution(sol.solution, notyet, sol.never)).tryFinalize
    }

    def upperBound(bounds: Seq[Type], t: Type): Seq[Constraint] = bound(bounds, t, EqJoinConstraint)
    def lowerBound(bounds: Seq[Type], t: Type): Seq[Constraint] = bound(bounds, t, EqMeetConstraint)

    def bound(bounds: Seq[Type], t: Type, mkBound: (Type, Type, Type) => Constraint) =
      if (bounds.size == 1)
        Seq(EqConstraint(t, bounds(0)))
      else if (bounds.size == 2)
       Seq(mkBound(bounds(0), bounds(1), t))
      else {
        val X = freshTVar()
        val hd = bounds.head
        val tl = bounds.tail
        val cons = lowerBound(tl, X)
        mkBound(hd, X, t) +: cons
      }

    def subst(s: TSubst) = SubConstraint(lower.subst(s), upper.subst(s))
  }
}
