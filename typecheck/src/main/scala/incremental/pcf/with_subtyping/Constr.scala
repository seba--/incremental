package incremental.pcf.with_subtyping

import incremental.ConstraintOps.Solution
import incremental.Type
import incremental.Type.TSubst
import incremental.pcf.{TNum, TVar}
import TypeOps._

/**
 * Created by oliver on 03.12.14.
 */
class Constr {
  type Require = Map[Symbol, Type]
  type NotYet = Map[Symbol, (LBound, UBound)]
  type Never = Set[Constraint]
  type Sol = (TSubst, NotYet, Never)

  private var _pos: Set[Symbol] = Set()
  private var _neg: Set[Symbol] = Set()

  def isPositive(a: Symbol): Boolean = _pos(a)
  def isNegative(a: Symbol): Boolean = _neg(a)
  def isBipolar(a: Symbol): Boolean = isPositive(a) && isNegative(a)
  def isProperPositive(a: Symbol): Boolean = _pos(a) && !_neg(a)
  def isProperNegative(a: Symbol): Boolean = !_pos(a) && _neg(a)

  private var _nextId = 0
  def freshTVar(positive: Boolean = true): TVar = {
    val v = TVar(Symbol("x$" + _nextId))
    _nextId += 1
    if (positive) _pos += v.x
    else _neg += v.x
    v
  }
  def freshBiVar(): TVar = {
    val res = freshTVar(true)
    _neg += res.x
    res
  }

  sealed trait Constraint {}
  case class Equal(expected: Type, actual: Type) extends Constraint
  case class Subtype(lower: Type, upper: Type) extends Constraint

  class CSet  {
    //invariant: values are ground types
    private[CSet] var solution: TSubst = Map()
    //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct
    private[CSet] var bounds: Map[Symbol, (LBound, UBound)] = Map().withDefaultValue((LBound(Set(), None), UBound(Set(), None)))
    private[CSet] var unsat: Set[Constraint] = Set()

    def <--(that: CSet): CSet = {
      unsat ++= that.unsat
      for((tv, (l1, u1)) <- that.bounds) {
        val (l2, u2) = bounds(tv)
        val merged = (l2 merge l1, u2 merge u1)
        bounds += tv -> merged
      }
      solution = mergeSubsts(solution, that.solution)
      this
    }

    def <--(c: Constraint): CSet = {
      c match {
        case Equal(t1, t2) =>
          val sigma = normalizeSub(t1, t2)
          val tau = normalizeSub(t2, t1)
          val sigmares = mergeSubsts(sigma, tau)
          extendSolution(sigmares)
        case Subtype(lower, upper) =>
          val sigma = normalizeSub(lower, upper)
          extendSolution(sigma)
      }
      this
    }

    def finalized: Sol = (solution, bounds, unsat)

    private[CSet] def normalizeSub(s: Type, t: Type): TSubst = (s,t) match {
      case (t1, t2) if t1 == t2 => Map()
      case (_, Top) => Map()
      case (TVar(a), TVar(b)) =>
        addLowerBound(b, s)
        addUpperBound(a, t)
        Map()
      case (TVar(a), t2) =>
        if (t2.occurs(a)) {
          never(Subtype(s, t))
          Map()
        }
        else
          addUpperBound(a, t2)
      case (t1, TVar(a)) =>
        if (t1.occurs(a)) {
          never(Subtype(s, t))
          Map()
        }
        else
          addLowerBound(a, t1)
      case (s1 -->: t1, s2 -->: t2) =>
        val sigma = normalizeSub(s2, s1)
        val tau = normalizeSub(t1, t2)
        mergeSubsts(sigma, tau)
      case _ =>
        never(Subtype(s, t))
        Map()
    }

    private[CSet] def extendSolution(subst: TSubst) = {
      var sol = subst
      while (sol.nonEmpty) {
        var temp = new CSet
        var nextsol: TSubst = Map()
        for ((tv, (lb, ub)) <- bounds) {
          val (newLb, newUb) = (lb.subst(sol), ub.subst(sol))
          val t = TVar(tv).subst(sol)
          val subs1 = mergeSubsts(for(tpe <- newLb.nonground)
                        yield temp.normalizeSub(tpe, t))
          val subs2 = mergeSubsts(for(tpe: Type <- newLb.ground.toSet)
                        yield temp.normalizeSub(tpe, t))
          val subs3 = mergeSubsts(for(tpe <- newUb.nonground)
                        yield temp.normalizeSub(t, tpe))
          val subs4 = mergeSubsts(for(tpe: Type <- newUb.ground.toSet)
                        yield temp.normalizeSub(t, tpe))
          nextsol = mergeSubsts(Set(nextsol, subs1, subs2, subs3, subs4))
        }
        bounds = temp.bounds
        unsat ++= temp.unsat
        solution ++= sol
        sol = nextsol
      }
    }

    private[CSet] def never(c: Constraint) = {
      unsat += c
    }
    private[CSet] def addLowerBound(v: Symbol, t: Type): TSubst = {
      val (lower, upper) = bounds(v)
      val newLower = lower.add(t)
      bounds += v -> (newLower, upper)
      if (isBipolar(v) && newLower.isGround && upper.isGround) {
        Map(v -> newLower.ground.get) //Note: we check later if the bounds are actually equal
      }
      else if (isProperPositive(v) && newLower.isGround)
        Map(v -> newLower.ground.get)
      else Map()
    }
    private[CSet] def addUpperBound(v: Symbol, t: Type): TSubst = {
      val (lower, upper) = bounds(v)
      val newUpper = upper.add(t)
      bounds += v -> (lower, newUpper)
      if (isBipolar(v) && lower.isGround && newUpper.isGround) {
        Map(v -> newUpper.ground.get) //Note: we check later if the bounds are actually equal
      }
      else if (isProperNegative(v) && newUpper.isGround)
        Map(v -> newUpper.ground.get)
      else Map()
    }

    private[CSet] def mergeSubsts(sigma: TSubst, tau: TSubst): TSubst = {
      for ((v, t1) <- sigma if tau.isDefinedAt(v) && t1 != tau(v))
        never(Equal(t1, tau(v)))
      tau ++ sigma
    }

    private[CSet] def mergeSubsts(ss: Set[TSubst]): TSubst =
      ss.fold(Map[Symbol, Type]()) { case (s, s1) => mergeSubsts(s, s1) }
  }
  case class LBound(nonground: Set[Type], ground: Option[Type]) {
    val isEmpty = nonground.isEmpty && !ground.isDefined
    val isGround = nonground.isEmpty && ground.isDefined
    def merge(that: LBound): LBound = LBound(nonground ++ that.nonground, (ground, that.ground) match {
      case (Some(t1), Some(t2)) => Some(t1 || t2)
      case (Some(t1), _) => ground
      case (_, Some(t2)) => that.ground
      case _ => None
    })
    def add(t: Type) = if (t.isGround) LBound(nonground, Some(ground.fold(t)(_ || t))) else LBound(nonground + t, ground)

    def subst(sigma: TSubst): LBound = {
      val (g, ng) = nonground.map(_.subst(sigma)).partition(_.isGround)
      val join = g.reduceOption(_ || _)
      LBound(ng, (ground, join) match {
        case (Some(t1), Some(t2)) => Some(t1 || t2)
        case (Some(t1), None) => Some(t1)
        case (None, Some(t2)) => Some(t2)
        case _ => None
      })
    }
  }

  case class UBound(nonground: Set[Type], ground: Option[Type]) {
    val isEmpty = nonground.isEmpty && !ground.isDefined
    val isGround = nonground.isEmpty && ground.isDefined
    def merge(that: UBound): UBound = UBound(nonground ++ that.nonground, (ground, that.ground) match {
      case (Some(t1), Some(t2)) => Some(t1 && t2)
      case (Some(t1), _) => ground
      case (_, Some(t2)) => that.ground
      case _ => None
    })
    def add(t: Type) = if (t.isGround) UBound(nonground, Some(ground.fold(t)(_ && t))) else UBound(nonground + t, ground)
    def subst(sigma: TSubst): LBound = {
      val (g, ng) = nonground.map(_.subst(sigma)).partition(_.isGround)
      val meet = g.reduceOption(_ && _)
      LBound(ng, (ground, meet) match {
        case (Some(t1), Some(t2)) => Some(t1 && t2)
        case (Some(t1), None) => Some(t1)
        case (None, Some(t2)) => Some(t2)
        case _ => None
      })
    }
  }
}
