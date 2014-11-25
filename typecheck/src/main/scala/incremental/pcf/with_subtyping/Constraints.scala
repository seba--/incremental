package incremental.pcf.with_subtyping



import incremental.Type._
import incremental.{Util, Type}
import incremental.pcf.{Constraint => _, TNum, TVar}
import TypeOps._

object Constraints {
  type Unsat = Set[(Type, Type)]
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
  val unconstr: Constraint = Between(Bot, Top)

  object Constraint {
    def normalize(lower: Type, arg: Type, upper: Type): (CSet, Unsat) = {
      val (c1, u1) = subtypeConstraints(lower, arg)
      val (c2, u2) = subtypeConstraints(arg, upper)
      (c1 && c2, u1 ++ u2)
    }

    def subtypeConstraints(s: Type, t: Type): (CSet, Unsat) = (s,t) match {
      case (_, Top) => (CSet(), Set())
      case (Bot, _) => (CSet(), Set())
      case (TVar(a), TVar(b)) if a == b => (CSet(), Set())
      case (TVar(a), t2) => (CSet(a -> Between(Bot, t2)), Set())
      case (t1, TVar(a)) => (CSet(a -> Between(t1, Top)), Set())
      case (TNum, TNum) => (CSet(), Set())
      case (s1 --> t1, s2 --> t2) =>
        val (c1, u1) = subtypeConstraints(s2, s1)
        val (c2, u2) = subtypeConstraints(t1, t2)
        (c1 && c2, u1 ++ u2)
      case _ => (CSet(), Set((s,t)))
    }

  }

  type CSet = Map[Symbol, Constraint]
  val empty: CSet = Map().withDefaultValue(unconstr)
  object CSet {
    def apply(cs: (Symbol, Constraint)*): CSet = Map((cs filter (_._2 != unconstr)): _*).withDefaultValue(unconstr)
  }
  implicit class CSetOps(val cs: CSet) extends AnyVal {
    def &&(that: CSet): CSet = {
      var res = cs
      for ((tv, c) <- that) {
        cs.get(tv) match {
          case Some(c1) =>
            res += tv -> (c && c1)
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

    def mergeReqMaps(r1: Require, r2: Require): (Require, CSet, Unsat) = {
      val (res, time) = Util.timed { _mergeReqMaps(r1, r2)  }
      mergeReqsTime += time
      res
    }

    def _mergeReqMaps(r1: Require, r2: Require): (Require, CSet, Unsat) = {
      var cset: CSet = CSet()
      var unsat: Unsat = Set()
      var req: Require = r1
      for ((v,tpe2) <- r2) {
        r1.get(v) match {
          case Some(tpe) =>
              val (c, u) = Constraint.normalize(tpe, tpe2, tpe)
              cset = cset && c
              unsat ++= u
          case None =>
            req += v -> tpe2
        }
      }
      (req, cset, unsat)
    }

    def mergeReqMaps(r1: Require, r2: Require, r3: Require, rs: Require*): (Require, CSet, Unsat) = {
      val (res, time) = Util.timed { _mergeReqMaps(r1, r2, r3, rs:_*)  }
      mergeReqsTime += time
      res
    }

    def _mergeReqMaps(r1: Require, r2: Require, r3: Require, rs: Require*): (Require, CSet, Unsat) = {
      (r2 +: r3 +: rs).foldLeft((r1, CSet(), Set[(Type,Type)]())) {
        case ((req, cset, unsat), req2) =>
          val (res, cset2, unsat2) = _mergeReqMaps(req, req2)
          (res, cset && cset2, unsat ++ unsat2)
      }
    }

    def solve(cset: CSet): (TSubst, CSet, Unsat) = {
      constraintCount += cset.size
      val (res, time) = Util.timed { _solve(cset) }
      constraintSolveTime += time
      res
    }


    def _solve(cset: CSet): (TSubst, CSet, Unsat) = {
      (Map(), cset, Set()) //TODO implement the solver
    }
  }
}
