package incremental.pcf.with_subtyping

import incremental.Type._
import incremental.{Util, Type}
import incremental.pcf.{Constraint => _, TNum, TVar}
import TypeOps._

object Constraints {
  private var _nextId = 0
  def freshTVar(): TVar = {
    val v = TVar(Symbol("x$" + _nextId))
    _nextId += 1
    v
  }
  def reset(): Unit = {
    _nextId = 0
  }

  type Unsat = Set[Constraint]
  type Require = Map[Symbol, Type]

  implicit class TypeOpsConstraints(val tpe: Type) extends AnyVal {
    def &(that: Type): (Type, CSet) = (tpe, that) match {
      case (t, Top) => (t, empty)
      case (Top, t) => (t, empty)
      case (Bot, _) | (_, Bot) => (Bot, empty)
      case (TNum, TNum) => (TNum, empty)
      case (TVar(x), TVar(y)) if x == y => (tpe, empty)
      case (TVar(x), TVar(y)) =>
        val tz = freshTVar()
        (tz, CSet(x -> Between(tz, Top), y -> Between(tz, Top)))
      case (TVar(x), t) =>
        val tz = freshTVar()
        (tz, CSet(x -> Between(tz, Top)) && Constraint.normalizeSub(tz, t, Top))
      case (t, TVar(x)) =>
        val tz = freshTVar()
        (tz, CSet(x -> Between(tz, Top)) && Constraint.normalizeSub(tz, t, Top))
      case (s1 -->: t1, s2 -->: t2) =>
        val (sr, cs) = s1 | s2
        val (tr, ct) = t1 & t2
        (sr -->: tr, cs && ct)
      case _ => (Bot, empty)
    }

    def |(that: Type): (Type, CSet) = (tpe, that) match {
      case (_, Top) | (Top, _) => (Top, empty)
      case (Bot, t) => (t, empty)
      case (t, Bot) => (t, empty)
      case (TNum, TNum) => (TNum, empty)
      case (TVar(x), TVar(y)) if x == y => (tpe, empty)
      case (TVar(x), TVar(y)) =>
        val tz = freshTVar()
        (tz, CSet(x -> Between(Bot, tz), y -> Between(Bot, tz)))
      case (TVar(x), t) =>
        val tz = freshTVar()
        (tz, CSet(x -> Between(Bot, tz)) && Constraint.normalizeSub(Bot, t, tz))
      case (t, TVar(x)) =>
        val tz = freshTVar()
        (tz, CSet(x -> Between(Bot, tz)) && Constraint.normalizeSub(Bot, t, tz))
      case (s1 -->: t1, s2 -->: t2) =>
        val (sr, cs) = s1 & s2
        val (tr, ct) = t1 | t2
        (sr -->: tr, cs && ct)
      case _ => (Top, empty)
    }

    def occurs(tv: Symbol): Boolean = tpe match {
      case TVar(x) if x == tv => true
      case s -->: t => s.occurs(tv) || t.occurs(tv)
      case _ => false
    }
  }

  case class ConstraintException(msg: String) extends RuntimeException(msg)
  sealed trait Constraint {
    def &&(that: Constraint): (Constraint, CSet)
    def subst(x: Symbol, t: Type): Constraint
  }
  case class Between(lower: Type, upper: Type) extends Constraint {
    def &&(that: Constraint) = that match {
      case Between(l1, u1) =>
        val (l, lc) = lower | l1
        val (u, uc) = upper & u1
        (if (l == u) Equal(l) else Between(l, u), lc && uc)
      case Equal(t) =>
        (that, Constraint.normalizeSub(lower, t, upper))
    }

    def subst(x: Symbol, t: Type) = Between(lower.subst(Map(x -> t)), upper.subst(Map(x -> t)))
  }
  case class Equal(tpe: Type) extends Constraint {
    def &&(that: Constraint) = that match {
      case Between(lower, upper) =>
        (this, Constraint.normalizeSub(lower, tpe, upper))
      case Equal(tpe2) =>
        (this, Constraint.normalizeEq(tpe, tpe2))
    }

    def subst(x: Symbol, t: Type) = Equal(tpe.subst(Map(x -> t)))
  }
  val unconstr: Constraint = Between(Bot, Top)

  object Constraint {
    def normalizeSub(lower: Type, arg: Type, upper: Type): CSet =
      subtypeConstraints(lower, arg) && subtypeConstraints(arg, upper)

    def normalizeEq(a: Type, b: Type): CSet = (a, b) match {
      case (t1, t2) if t1 == t2 => empty
      case (TVar(x), TVar(y)) => CSet(x -> Equal(TVar(y)), y -> Equal(TVar(x)))
      case (TVar(x), t2) =>
        if (t2.occurs(x))
          throw ConstraintException(s"Unsatisfiable equality $a = $b. Variable $x occurs in $b")
        CSet(x -> Equal(t2))
      case (t1, TVar(x)) =>
        if (t1.occurs(x))
          throw ConstraintException(s"Unsatisfiable equality $a = $b. Variable $x occurs in $a")
        CSet(x -> Equal(t1))
      case (s1 -->: t1, s2 -->: t2) =>
        normalizeEq(s1, s2) && normalizeEq(t1, t2)
      case _ => throw ConstraintException(s"Unsatisfiable equality between types $a and $b")
    }

    def subtypeConstraints(s: Type, t: Type): CSet = (s,t) match {
      case (t1, t2) if t1 == t2 => empty
      case (_, Top) => empty
      case (Bot, _) => empty
      case (TVar(a), t2) =>
        if (t2.occurs(a))
          throw ConstraintException(s"Unsatisfiable constraint $s <: $t. Variable $a occurs in $t")
        CSet(a -> Between(Bot, t2))
      case (t1, TVar(a)) =>
        if (t1.occurs(a))
          throw ConstraintException(s"Unsatisfiable constraint $s <: $t. Variable $a occurs in $s")
        CSet(a -> Between(t1, Top))
      case (s1 -->: t1, s2 -->: t2) =>
        subtypeConstraints(s2, s1) && subtypeConstraints(t1, t2)
      case _ => throw ConstraintException(s"Type $s is not a subtype of $t")
    }
  }

  type CSet = Map[Symbol, Constraint]
  val empty: CSet = Map().withDefaultValue(unconstr)
  object CSet {
    def apply(cs: (Symbol, Constraint)*): CSet = {
      var res: CSet = empty
      for ((tv, c) <- cs if c != unconstr) {
        res = res.insert(tv, c)
      }
      res
    }
  }
  implicit class CSetOps(val cs: CSet) extends AnyVal {
    def &&(that: CSet): CSet = extend(that)

    private[Constraints] def insert(x: Symbol, c: Constraint): CSet = {
      cs.get(x) match {
        case Some(c1) =>
          val (c2, unres) = c && c1
          cs.updated(x, c2).extend(unres)
        case None =>
          cs.updated(x, c)
      }
    }

    private[Constraints] def extend(newcs: CSet): CSet = {
      var res = cs
      for ((x, c) <- newcs if c != unconstr) {
        c match {
          case e@Equal(t) =>
            res = res.subst(x, e)
          case Between(l, u) if l == u=>
            res = res.subst(x, Equal(u))
          case _ =>
            res = res.insert(x, c)
        }
      }
      res
    }

    private[Constraints] def subst(x: Symbol, c: Equal): CSet = {
      val (eq@Equal(t), nuCons) = cs(x) && c
      var res: CSet = cs
      if (cs(x) != eq) {
        res = cs.updated(x, eq)
        for ((v,_) <- cs if v != x)
          res = res._subst(v, x, t)
      }
      res.extend(nuCons)
    }

    private[Constraints] def _subst(y: Symbol, x: Symbol, t: Type): CSet = {
      //assumes x != y
      cs.get(y) match {
        case Some(c) =>
          cs.updated(y, c.subst(x, t))
        case None =>
          cs
      }
    }

    def finalized: TSubst = cs.mapValues {
      case Equal(t) => t
      case Between(lower, upper) if lower < upper =>  //TODO check if this is always sound
        lower
      case _ =>
        throw ConstraintException(s"There were unresolved subtype constraints in solution $cs")
    }
  }

  class Solver {
    var constraintCount = 0
    var mergeReqsTime = 0.0
    var constraintSolveTime = 0.0
    var mergeSolutionTime = 0.0

    def mergeReqMaps(r1: Require, r2: Require): (Require, CSet) = {
      val (res, time) = Util.timed { _mergeReqMaps(r1, r2) }
      mergeReqsTime += time
      res
    }

    def _mergeReqMaps(r1: Require, r2: Require): (Require, CSet) = {
      var cset: CSet = empty
      var req: Require = r1
      for ((v,tpe2) <- r2) {
        r1.get(v) match {
          case Some(tpe) =>
              val c = Constraint.normalizeEq(tpe, tpe2)
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
  }
}
