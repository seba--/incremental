package incremental.pcf.with_subtyping

import incremental.Type._
import incremental.{Util, Type}
import incremental.pcf.{Constraint => _, TNum, TVar}
import TypeOps._

object Constraints {
  type Unsat = Set[Constraint]
  type Require = Map[Symbol, Type]

  sealed trait Constraint {
    def &&(that: Constraint): Constraint
  }
  case class Between(lower: Type, upper: Type) extends Constraint {
    def &&(that: Constraint) = that match {
      case Between(l1, u1) =>
        Between(lower | l1, upper & u1)
    }
  }
  case class Equal(tpe: Type) extends Constraint {
    def &&(that: Constraint) = ??? //TODO
  }
  val unconstr: Constraint = Between(Bot, Top)

  object Constraint {
    def normalizeSub(lower: Type, arg: Type, upper: Type): CSet =
      subtypeConstraints(lower, arg) && subtypeConstraints(arg, upper)
    
    def normalizeEq(a: Type, b: Type): CSet = (a, b) match {
      case (t1, t2) if t1 == t2 => empty
      case (TVar(x), TVar(y)) => CSet(x -> Equal(TVar(y)), y -> Equal(TVar(x)))
      case (TVar(x), t2) => CSet(x -> Equal(t2))
      case (t1, TVar(x)) => CSet(x -> Equal(t1))
      case (s1 -->: t1, s2 -->: t2) => normalizeEq(s1, s2) && normalizeEq(s2, t2)
    }

    def subtypeConstraints(s: Type, t: Type): CSet = (s,t) match {
      case (t1, t2) if t1 == t2 => empty
      case (_, Top) => empty
      case (Bot, _) => empty
      case (TVar(a), t2) => CSet(a -> Between(Bot, t2))
      case (t1, TVar(a)) => CSet(a -> Between(t1, Top))
      case (s1 -->: t1, s2 -->: t2) =>
        subtypeConstraints(s2, s1) && subtypeConstraints(t1, t2)
    }
  }

  type CSet = Map[Symbol, Set[Constraint]]
  val empty: CSet = Map().withDefaultValue(Set(unconstr))
  object CSet {
    def apply(cs: (Symbol, Constraint)*): CSet = {
      var res: CSet = Map()
      for ((tv, c) <- cs) {
        res.get(tv) match {
          case None => res += tv -> Set(c)
          case Some(cset) => res += tv -> (cset + c)
        }
      }
      res
    }
  }
  implicit class CSetOps(val cs: CSet) extends AnyVal {
    def &&(that: CSet): CSet = {
      var res = cs
      for ((tv, c) <- that) {
        cs.get(tv) match {
          case Some(c1) =>
            res += tv -> (c ++ c1)
          case None =>
            res += tv -> c
        }
      }
      res
    }
  }

  class Solver {
    private var _nextId = 0
    def freshTVar(): TVar = {
      val v = TVar(Symbol("x$" + _nextId))
      _nextId += 1
      v
    }

    var constraintCount = 0
    var mergeReqsTime = 0.0
    var constraintSolveTime = 0.0
    var mergeSolutionTime = 0.0

    def mergeReqMaps(r1: Require, r2: Require): (Require, CSet) = {
      val (res, time) = Util.timed { _mergeReqMaps(r1, r2)  }
      mergeReqsTime += time
      res
    }

    def _mergeReqMaps(r1: Require, r2: Require): (Require, CSet) = {
      var cset: CSet = empty
      var req: Require = r1
      for ((v,tpe2) <- r2) {
        r1.get(v) match {
          case Some(tpe) =>
              val c = Constraint.normalizeSub(tpe, tpe2, tpe)
              cset = cset && c
          case None =>
            req += v -> tpe2
        }
      }
      (req, cset)
    }

    def mergeReqMaps(r1: Require, r2: Require, r3: Require, rs: Require*): (Require, CSet) = {
      val (res, time) = Util.timed { _mergeReqMaps(r1, r2, r3, rs:_*)  }
      mergeReqsTime += time
      res
    }

    def _mergeReqMaps(r1: Require, r2: Require, r3: Require, rs: Require*): (Require, CSet) = {
      (r2 +: r3 +: rs).foldLeft((r1, empty)) {
        case ((req, cset), req2) =>
          val (res, cset2) = _mergeReqMaps(req, req2)
          (res, cset && cset2)
      }
    }

    def solve(cset: CSet): (TSubst, CSet) = {
      constraintCount += cset.size
      val (res, time) = Util.timed { _solve(cset) }
      constraintSolveTime += time
      res
    }


    def _solve(cset: CSet): (TSubst, CSet) = {
      (Map(), cset) //TODO implement the solver
    }
  }
}
