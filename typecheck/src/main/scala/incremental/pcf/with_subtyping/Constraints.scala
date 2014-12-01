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
          val Xmeet = freshTVar()
          mcons = SubConstraint(r1, Xmeet) +: SubConstraint(r2, Xmeet) +: mcons
          mreqs += x -> Xmeet
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

//object Constraints {
//  private var _nextId = 0
//  def freshTVar(): TVar = {
//    val v = TVar(Symbol("x$" + _nextId))
//    _nextId += 1
//    v
//  }
//  def reset(): Unit = {
//    _nextId = 0
//  }
//
//  type Unsat = Set[Constraint]
//  type Require = Map[Symbol, Type]
//
//  implicit class TypeOpsConstraints(val tpe: Type) extends AnyVal {
//    def &(that: Type): (Type, CSet) = (tpe, that) match {
//      case (t, Top) => (t, empty)
//      case (Top, t) => (t, empty)
//      case (Bot, _) | (_, Bot) => (Bot, empty)
//      case (TNum, TNum) => (TNum, empty)
//      case (TVar(x), TVar(y)) if x == y => (tpe, empty)
//      case (TVar(x), TVar(y)) =>
//        val tz = freshTVar()
//        (tz, CSet(x -> Between(tz, Top), y -> Between(tz, Top)))
//      case (TVar(x), t) =>
//        val tz = freshTVar()
//        (tz, CSet(x -> Between(tz, Top)) && Constraint.normalizeSub(tz, t, Top))
//      case (t, TVar(x)) =>
//        val tz = freshTVar()
//        (tz, CSet(x -> Between(tz, Top)) && Constraint.normalizeSub(tz, t, Top))
//      case (s1 -->: t1, s2 -->: t2) =>
//        val (sr, cs) = s1 | s2
//        val (tr, ct) = t1 & t2
//        (sr -->: tr, cs && ct)
//      case _ => (Bot, empty)
//    }
//
//    def |(that: Type): (Type, CSet) = (tpe, that) match {
//      case (_, Top) | (Top, _) => (Top, empty)
//      case (Bot, t) => (t, empty)
//      case (t, Bot) => (t, empty)
//      case (TNum, TNum) => (TNum, empty)
//      case (TVar(x), TVar(y)) if x == y => (tpe, empty)
//      case (TVar(x), TVar(y)) =>
//        val tz = freshTVar()
//        (tz, CSet(x -> Between(Bot, tz), y -> Between(Bot, tz)))
//      case (TVar(x), t) =>
//        val tz = freshTVar()
//        (tz, CSet(x -> Between(Bot, tz)) && Constraint.normalizeSub(Bot, t, tz))
//      case (t, TVar(x)) =>
//        val tz = freshTVar()
//        (tz, CSet(x -> Between(Bot, tz)) && Constraint.normalizeSub(Bot, t, tz))
//      case (s1 -->: t1, s2 -->: t2) =>
//        val (sr, cs) = s1 & s2
//        val (tr, ct) = t1 | t2
//        (sr -->: tr, cs && ct)
//      case _ => (Top, empty)
//    }
//  }
//
//  case class ConstraintException(msg: String) extends RuntimeException(msg)
//  sealed trait Constraint {
//    def &&(that: Constraint): (Constraint, CSet)
//    def subst(x: Symbol, t: Type): Constraint
//  }
//  case class Between(lower: Type, upper: Type) extends Constraint {
//    def &&(that: Constraint) = that match {
//      case Between(l1, u1) =>
//        val (l, lc) = lower | l1
//        val (u, uc) = upper & u1
//        (if (l == u) Equal(l) else Between(l, u), lc && uc)
//      case Equal(t) =>
//        (that, Constraint.normalizeSub(lower, t, upper))
//    }
//
//    def subst(x: Symbol, t: Type) = Between(lower.subst(Map(x -> t)), upper.subst(Map(x -> t)))
//  }
//  case class Equal(tpe: Type) extends Constraint {
//    def &&(that: Constraint) = that match {
//      case Between(lower, upper) =>
//        (this, Constraint.normalizeSub(lower, tpe, upper))
//      case Equal(tpe2) =>
//        (this, Constraint.normalizeEq(tpe, tpe2))
//    }
//
//    def subst(x: Symbol, t: Type) = Equal(tpe.subst(Map(x -> t)))
//  }
//  val unconstr: Constraint = Between(Bot, Top)
//
//  object Constraint {
//    def normalizeSub(lower: Type, arg: Type, upper: Type): CSet =
//      subtypeConstraints(lower, arg) && subtypeConstraints(arg, upper)
//
//    def normalizeEq(a: Type, b: Type): CSet = a.unify(b) match {
//      case None => throw ConstraintException(s"Unsatisfiable equality between types $a and $b")
//      case Some(s) => s.mapValues(Equal(_))
//    }
//
//    def subtypeConstraints(s: Type, t: Type): CSet = (s,t) match {
//      case (t1, t2) if t1 == t2 => empty
//      case (_, Top) => empty
//      case (Bot, _) => empty
//      case (TVar(a), t2) =>
//        if (t2.occurs(a))
//          throw ConstraintException(s"Unsatisfiable constraint $s <: $t. Variable $a occurs in $t")
//        CSet(a -> Between(Bot, t2))
//      case (t1, TVar(a)) =>
//        if (t1.occurs(a))
//          throw ConstraintException(s"Unsatisfiable constraint $s <: $t. Variable $a occurs in $s")
//        CSet(a -> Between(t1, Top))
//      case (s1 -->: t1, s2 -->: t2) =>
//        subtypeConstraints(s2, s1) && subtypeConstraints(t1, t2)
//      case _ => throw ConstraintException(s"Type $s is not a subtype of $t")
//    }
//  }
//
//  type CSet = Map[Symbol, Constraint]
//  val empty: CSet = Map().withDefaultValue(unconstr)
//  object CSet {
//    def apply(cs: (Symbol, Constraint)*): CSet = {
//      var res: CSet = empty
//      for ((tv, c) <- cs if c != unconstr) {
//        res = res.insert(tv, c)
//      }
//      res
//    }
//  }
//  implicit class CSetOps(val cs: CSet) extends AnyVal {
//    def &&(that: CSet): CSet = extend(that)
//
//    private[Constraints] def insert(x: Symbol, c: Constraint): CSet = {
//      cs.get(x) match {
//        case Some(c1) =>
//          val (c2, unres) = c && c1
//          cs.updated(x, c2).extend(unres)
//        case None =>
//          cs.updated(x, c)
//      }
//    }
//
//    private[Constraints] def extend(newcs: CSet): CSet = {
//      var res = cs
//      for ((x, c) <- newcs if c != unconstr) {
//        c match {
//          case e@Equal(t) =>
//            res = res.subst(x, e)
//          case Between(l, u) if l == u=>
//            res = res.subst(x, Equal(u))
//          case _ =>
//            res = res.insert(x, c)
//        }
//      }
//      res
//    }
//
//    private[Constraints] def subst(x: Symbol, c: Equal): CSet = {
//      val (eq@Equal(t), nuCons) = cs(x) && c
//      var res: CSet = cs
//      if (cs(x) != eq) {
//        res = cs.updated(x, eq)
//        for ((v,_) <- cs if v != x)
//          res = res._subst(v, x, t)
//      }
//      res.extend(nuCons)
//    }
//
//    private[Constraints] def _subst(y: Symbol, x: Symbol, t: Type): CSet = {
//      //assumes x != y
//      cs.get(y) match {
//        case Some(c) =>
//          cs.updated(y, c.subst(x, t))
//        case None =>
//          cs
//      }
//    }
//
//    def finalized: TSubst = cs.mapValues {
//      case Equal(t) => t
//      case Between(lower, upper) if lower < upper =>  //TODO check if this is always sound
//        lower
//      case _ =>
//        throw ConstraintException(s"There were unresolved subtype constraints in solution $cs")
//    }
//  }
//
//  class Solver {
//    var constraintCount = 0
//    var mergeReqsTime = 0.0
//    var constraintSolveTime = 0.0
//    var mergeSolutionTime = 0.0
//
//    def mergeReqMaps(r1: Require, r2: Require): (Require, CSet) = {
//      val (res, time) = Util.timed { _mergeReqMaps(r1, r2) }
//      mergeReqsTime += time
//      res
//    }
//
//    def _mergeReqMaps(r1: Require, r2: Require): (Require, CSet) = {
//      var cset: CSet = empty
//      var req: Require = r1
//      for ((v,tpe2) <- r2) {
//        r1.get(v) match {
//          case Some(tpe) =>
//              val c = Constraint.normalizeEq(tpe, tpe2)
//              cset = cset && c
//          case None =>
//            req += v -> tpe2
//        }
//      }
//      (req, cset)
//    }
//
//    def mergeReqMaps(r1: Require, r2: Require, r3: Require, rs: Require*): (Require, CSet) = {
//      val (res, time) = Util.timed { _mergeReqMaps(r1, r2, r3, rs:_*)  }
//      mergeReqsTime += time
//      res
//    }
//
//    def _mergeReqMaps(r1: Require, r2: Require, r3: Require, rs: Require*): (Require, CSet) = {
//      (r2 +: r3 +: rs).foldLeft((r1, empty)) {
//        case ((req, cset), req2) =>
//          val (res, cset2) = _mergeReqMaps(req, req2)
//          (res, cset && cset2)
//      }
//    }
//  }
//}
