package incremental.pcf.with_subtyping

import incremental.{Util, Type}
import incremental.Type.TSubst
import incremental.pcf.{TNum, TVar}
import TypeOps._

/**
 * Created by oliver on 03.12.14.
 */



class Constr {
  sealed trait Constraint {}
  case class Subtype(lower: Type, upper: Type) extends Constraint
  //the following are not actually used, they just serve for error reporting
  case class Equal(expected: Type, actual: Type) extends Constraint
  case class Join(ts: Set[Type]) extends Constraint
  case class Meet(ts: Set[Type]) extends Constraint

  incremental.ConstraintOps.constraintCount = 0
  incremental.ConstraintOps.constraintSolveTime = 0
  incremental.ConstraintOps.mergeSolutionTime = 0
  def constraintCount = incremental.ConstraintOps.constraintCount
  def constraintSolveTime = incremental.ConstraintOps.constraintSolveTime
  def mergeSolutionTime = incremental.ConstraintOps.mergeSolutionTime
  var mergeReqsTime = 0.0

  type Require = Map[Symbol, Type]
  type NotYet = Map[Symbol, (LBound, UBound)]
  type Never = Set[Constraint]
  type Sol = (TSubst, NotYet, Never)


  def mergeReqMaps(reqs1: Require, reqs2: Require) = {
    val (res, time) = Util.timed(_mergeReqMaps(reqs1, reqs2))
    mergeReqsTime += time
    res
  }

  def _mergeReqMaps(reqs1: Require, reqs2: Require): (CSet, Require) = {
    var mcons = CSet()
    var mreqs = reqs1
    for ((x, r2) <- reqs2)
      reqs1.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          val Xmeet = freshTVar(false)
          mcons <-- Subtype(Xmeet, r1) <-- Subtype(Xmeet, r2)
          mreqs += x -> Xmeet
      }
    (mcons, mreqs)
  }

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


  object CSet {
    def apply(): CSet = new CSet
  }

  class CSet  {
    //TODO make this immutable
    //invariant: values are ground types
    private[CSet] var _solution: TSubst = Map()
    //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct
    private[CSet] var bounds: Map[Symbol, (LBound, UBound)] = Map().withDefaultValue((LBound(Set(), None), UBound(Set(), None)))
    private[CSet] var unsat: Set[Constraint] = Set()

    def solution: TSubst = _solution

    def <--(that: CSet): CSet = {
      unsat ++= that.unsat
      for((tv, (l1, u1)) <- that.bounds) {
        val (l2, u2) = bounds(tv)
        val (newL, errorl) = l2 merge l1
        val (newU, erroru) = u2 merge u1
        if(errorl.nonEmpty)
          never(Join(errorl))
        if(erroru.nonEmpty)
          never(Meet(erroru))
        val merged = (newL, newU)
        bounds += tv -> merged
      }
      _solution = mergeSubsts(_solution, that._solution)   //TODO verify if it is safe to just join the maps
      this
    }

    //add and solve immediately
    def <--(c: Constraint): CSet = {
      c match {
        case Equal(t1, t2) =>
          normalizeSub(t1, t2)
          normalizeSub(t2, t1)
          saturateSolution()
        case Subtype(lower, upper) =>
          normalizeSub(lower, upper)
          saturateSolution()
        case _ => ???
      }
      this
    }

    //add but do not solve immediately
    def +(c: Constraint): CSet = {
      c match {
        case Equal(t1, t2) =>
          normalizeSub(t1, t2)
          normalizeSub(t2, t1)
        case Subtype(lower, upper) =>
          normalizeSub(lower, upper)
        case _ => ???
      }
      this
    }

    def tryFinalize: Sol = {
      //set upper bounds of negative vars to Top if still undetermined and solve
      bounds = bounds.map {
        case (tv, (lower, upper)) if isNegative(tv) && !upper.isGround =>
          val (newUpper, _) = upper.add(Top)
          (tv, (lower, newUpper))
        case x => x
      }
      saturateSolution()
      (_solution, bounds, unsat)
    }

    private[CSet] def normalizeSub(s: Type, t: Type): Unit = (s,t) match {
      case (t1, t2) if t1 == t2 =>
      case (_, Top) =>
      case (TVar(a), TVar(b)) =>
        addLowerBound(b, s)
        addUpperBound(a, t)
      case (TVar(a), t2) =>
        if (t2.occurs(a))
          never(Subtype(s, t))
        else
          addUpperBound(a, t2)
      case (t1, TVar(a)) =>
        if (t1.occurs(a))
          never(Subtype(s, t))
        else
          addLowerBound(a, t1)
      case (s1 -->: t1, s2 -->: t2) =>
        normalizeSub(s2, s1)
        normalizeSub(t1, t2)
      case _ =>
        never(Subtype(s, t))
    }

    private[CSet] def saturateSolution() = {
      var sol = solveOnce()
      while (sol.nonEmpty) {
        var temp = new CSet
        for ((tv, (lb, ub)) <- bounds) {
          val ((newLb, errorl), (newUb, erroru)) = (lb.subst(sol), ub.subst(sol))
          if(errorl.nonEmpty)
            never(Join(errorl))
          if(erroru.nonEmpty)
            never(Meet(erroru))
          val t = TVar(tv).subst(sol)
          for(tpe <- newLb.ground.toSet ++ newLb.nonground)
            temp.normalizeSub(tpe, t)
          for(tpe <- newUb.ground.toSet ++ newUb.nonground)
            temp.normalizeSub(t, tpe)
        }
        bounds = temp.bounds
        unsat ++= temp.unsat
        _solution ++= sol
        sol = solveOnce()
      }
    }

    private[CSet] def solveOnce(): TSubst = {
      var sol: TSubst = Map()
      for ((tv, (lower, upper)) <- bounds) {
        if (isBipolar(tv) && lower.isGround && upper.isGround)
          sol += tv -> lower.ground.get
        else if (isProperPositive(tv) && lower.isGround)
          sol += tv -> lower.ground.get
        else if (isProperNegative(tv) && upper.isGround)
          sol += tv -> upper.ground.get
      }
      sol
    }

    private[CSet] def never(c: Constraint) = unsat += c

    private[CSet] def addLowerBound(v: Symbol, t: Type) = {
      val (lower, upper) = bounds(v)
      val (newLower, error) = lower.add(t)
      if (error.nonEmpty)
        never(Join(error))
      bounds += v -> (newLower, upper)
      val changed = if (t.isGround) newLower.ground.get else t
      for(t2 <- upper.ground.toSet ++ upper.nonground)
        normalizeSub(changed, t2)
    }

    private[CSet] def addUpperBound(v: Symbol, t: Type) = {
      val (lower, upper) = bounds(v)
      val (newUpper, error) = upper.add(t)
      if (error.nonEmpty)
        never(Meet(error))
      bounds += v -> (lower, newUpper)
      val changed = if (t.isGround) newUpper.ground.get else t
      var subst: TSubst = Map()
      for(t2 <- lower.ground.toSet ++ lower.nonground)
        normalizeSub(t2, changed)
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

    def merge(that: LBound): (LBound, Set[Type]) = {
      (ground, that.ground) match {
        case (Some(t1), Some(t2)) =>
          t1 || t2 match {
            case None => (LBound(nonground ++ that.nonground, ground), Set(t1,t2))
            case lub => (LBound(nonground ++ that.nonground, lub), Set())
          }
        case _ => (LBound(nonground ++ that.nonground, ground.orElse(that.ground)), Set())
      }
    }

    //2nd component: set of types which have undefined least upper bound
    def add(t: Type): (LBound, Set[Type]) =
      if (t.isGround) {
        val lub = if (ground.isDefined) t || ground.get else Some(t)
        val error = if (lub.isDefined) Set[Type]() else Set(t, ground.get)
        (LBound(nonground, lub), error)
      }
      else (LBound(nonground + t, ground), Set())

    def subst(sigma: TSubst): (LBound, Set[Type]) = {
      val (g, ng) = nonground.map(_.subst(sigma)).partition(_.isGround)
      if (g.isEmpty)
        (LBound(ng, ground), Set())
      else {
        val lub: Option[Type] = g.map(Some(_)).reduce( (a: Option[Type], b: Option[Type]) => (a,b) match {
          case (Some(t1), Some(t2)) => (t1 || t2)
          case _ => None
        })

        if(lub.isDefined) {
          (lub, ground) match {
            case (Some(t1), Some(t2)) =>
              t1 || t2 match {
                case None => (LBound(ng, ground), g + ground.get)
                case lub => (LBound(ng, lub), Set())
              }
            case _ => (LBound(ng, ground.orElse(lub)), Set())
          }
        }
        else (LBound(ng, ground), g)
      }
    }
  }

  case class UBound(nonground: Set[Type], ground: Option[Type]) {
    val isEmpty = nonground.isEmpty && !ground.isDefined
    val isGround = nonground.isEmpty && ground.isDefined

    def merge(that: UBound): (UBound, Set[Type]) = {
      (ground, that.ground) match {
        case (Some(t1), Some(t2)) =>
          t1 && t2 match {
            case None => (UBound(nonground ++ that.nonground, ground), Set(t1, t2))
            case meet => (UBound(nonground ++ that.nonground, meet), Set())
          }
        case _ => (UBound(nonground ++ that.nonground, ground.orElse(that.ground)), Set())
      }
    }

    //2nd component: set of types which have undefined greatest lower bound
    def add(t: Type): (UBound, Set[Type]) =
      if (t.isGround) {
        val lub = if (ground.isDefined) t && ground.get else Some(t)
        val error = if (lub.isDefined) Set[Type]() else Set(t, ground.get)
        (UBound(nonground, lub), error)
      }
      else (UBound(nonground + t, ground), Set())

    def subst(sigma: TSubst): (UBound, Set[Type]) = {
      val (g, ng) = nonground.map(_.subst(sigma)).partition(_.isGround)
      if (g.isEmpty)
        (UBound(ng, ground), Set())
      else {
        val meet: Option[Type] = g.map(Some(_)).reduce( (a: Option[Type], b: Option[Type]) => (a,b) match {
          case (Some(t1), Some(t2)) => (t1 && t2)
          case _ => None
        })

        if(meet.isDefined) {
          (meet, ground) match {
            case (Some(t1), Some(t2)) =>
              t1 && t2 match {
                case None => (UBound(ng, ground), g + ground.get)
                case meet => (UBound(ng, meet), Set())
              }
            case _ => (UBound(ng, ground.orElse(meet)), Set())
          }
        }
        else (UBound(ng, ground), g)
      }
    }
  }
}
