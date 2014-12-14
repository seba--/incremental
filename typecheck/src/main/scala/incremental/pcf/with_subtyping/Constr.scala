package incremental.pcf.with_subtyping

import incremental.{ConstraintSystem, ConstraintDefs, Util, Statistics}
import TypeOps._
import incremental.pcf.with_subtyping.Type.Companion._

/**
 * Created by oliver on 03.12.14.
 */
class SubtypeSystem extends ConstraintSystem[Type, CD.type] {
  val defs = CD
  import defs._

  def _mergeReqMaps(reqs1: Requirements, reqs2: Requirements) = {
    var mcons = Seq[Meet]()
    var mreqs = reqs1
    for ((x, r2) <- reqs2)
      reqs1.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          val Xmeet = gen.freshUVar(false)
          mcons = Meet(Xmeet, Set(r1, r2)) +: mcons
          mreqs += x -> Xmeet
      }
    (mcons, mreqs)
  }

  def emptyCSet = new CSet()

  implicit val gen: Gen = new Gen
  implicit val stats: Statistics = new Statistics
}

object CD extends ConstraintDefs[Type] {
  sealed trait Constraint
  case class Subtype(lower: Type, upper: Type) extends Constraint
  case class Equal(expected: Type, actual: Type) extends Constraint
  case class Join(target: Type, ts: Set[Type]) extends Constraint
  case class Meet(target: Type, ts: Set[Type]) extends Constraint

  class Gen extends GenBase {
    type V = UVar
    private var _pos: Set[Symbol] = Set()
    private var _neg: Set[Symbol] = Set()

    def isPositive(a: Symbol): Boolean = _pos(a)
    def isNegative(a: Symbol): Boolean = _neg(a)
    def isBipolar(a: Symbol): Boolean = isPositive(a) && isNegative(a)
    def isProperPositive(a: Symbol): Boolean = _pos(a) && !_neg(a)
    def isProperNegative(a: Symbol): Boolean = !_pos(a) && _neg(a)

    private var _nextId = 0
    def freshUVar(positive: Boolean): UVar = {
      val v = UVar(Symbol("x$" + _nextId))
      _nextId += 1
      if (positive) _pos += v.x
      else _neg += v.x
      v
    }
    def freshBiVar(): UVar = {
      val res = freshUVar(true)
      _neg += res.x
      res
    }

    def freshUVar() = freshUVar(true)
  }
  type NotYetSolvable = Map[Symbol, (LBound, UBound)]
  type Unsolvable = Set[Constraint]

  class CSet(implicit val stat: Statistics, val gen: Gen) extends CSetAlg[CSet] {
    import gen._
    import stat._
    //invariant: values are ground types
    private[CSet] var _solution: TSubst = Map()
    //invariant: there is at most one ground type in each bound, each key does not occur in its bounds, keys of solution and bounds are distinct
    private[CSet] var bounds: Map[Symbol, (LBound, UBound)] = Map().withDefaultValue((LBound(Set(), None), UBound(Set(), None)))
    private[CSet] var unsat: Set[Constraint] = Set()


    def trySolve = {
      val res = copy
      res.saturateSolution()
      res
    }
    def isSolved = bounds.isEmpty && unsat.isEmpty
    def solution = (_solution, bounds, unsat)
    def notyet = bounds
    def never = unsat
    def substitution = _solution

    def ++(that: CSet) = {
      val res = copy
      res.unsat ++= that.unsat
      for((tv, (l1, u1)) <- that.bounds) {
        val (l2, u2) = res.bounds(tv)
        val (newL, errorl) = l2 merge l1
        val (newU, erroru) = u2 merge u1
        if(errorl.nonEmpty)
          res.gameOver(Join(UVar(tv), errorl))
        if(erroru.nonEmpty)
          res.gameOver(Meet(UVar(tv), erroru))
        val merged = (newL, newU)
        res.bounds += tv -> merged
      }
      res._solution = res._solution ++ that._solution
      res
    }

    def ++(cs: Iterable[Constraint]) = {
      val res = copy
      for(c <- cs)
        res.add(c)
      res
    }

   /* //add and solve immediately
    def <--(c: Constraint): CSet = {
      val res = copy
      c match {
        case Equal(t1, t2) =>
          res.normalizeSub(t1, t2)
          res.normalizeSub(t2, t1)
          res.saturateSolution()
        case Subtype(lower, upper) =>
          res.normalizeSub(lower, upper)
          res.saturateSolution()
        case _ =>
      }
      res
    }*/

    //add but do not solve immediately
    def +(c: Constraint): CSet = {
      val res = copy
      res.add(c)
    }

    private[CSet] def add(c: Constraint): CSet = {
      c match {
        case Equal(t1, t2) =>
          normalizeSub(t1, t2)
          normalizeSub(t2, t1)
        case Subtype(lower, upper) =>
          normalizeSub(lower, upper)
        case Join(target, ts) =>
          for(t <- ts)
            normalizeSub(t, target)
        case Meet(target, ts) =>
          for(t <- ts)
            normalizeSub(target, t)
        case _ => ???
      }
      this
    }

    def tryFinalize: CSet = {
      val res = copy
      //set upper bounds of negative vars to Top if still undetermined and solve
      res.bounds = bounds.map {
        case (tv, (lower, upper)) if isNegative(tv) && !upper.isGround =>
          val (newUpper, _) = upper.add(Top)
          (tv, (lower, newUpper))
        case x => x
      }
      res.saturateSolution()
      res
    }

    def copy: CSet = {
      val res = new CSet()
      res._solution = this._solution
      res.bounds = this.bounds
      res.unsat = this.unsat
      res
    }

    private[CSet] def normalizeSub(s: Type, t: Type): Unit = (s,t) match {
      case (t1, t2) if t1 == t2 =>
      case (_, Top) =>
      case (UVar(a), UVar(b)) =>
        if (isNegative(a))
          addUpperBound(a, t)
        else {
          addUpperBound(a, t)
          addLowerBound(b, s)
        }

      case (UVar(a), t2) =>
        if (t2.occurs(a))
          gameOver(Subtype(s, t))
        else
          addUpperBound(a, t2)
      case (t1, UVar(a)) =>
        if (t1.occurs(a))
          gameOver(Subtype(s, t))
        else
          addLowerBound(a, t1)
      case (TNum, TNumeric) =>
      case (TFloat, TNumeric) =>
      case (s1 -->: t1, s2 -->: t2) =>
        normalizeSub(s2, s1)
        normalizeSub(t1, t2)
      case _ =>
        gameOver(Subtype(s, t))
    }

    private[CSet] def saturateSolution() = {
      var sol = solveOnce
      while (sol.nonEmpty) {
        var temp = new CSet()
        for ((tv, (lb, ub)) <- bounds) {
          val ((newLb, errorl), (newUb, erroru)) = (lb.subst(sol), ub.subst(sol))
          if(errorl.nonEmpty)
            gameOver(Join(UVar(tv).subst(sol), errorl))
          if(erroru.nonEmpty)
            gameOver(Meet(UVar(tv).subst(sol), erroru))
          val t = UVar(tv).subst(sol)
          for(tpe <- newLb.ground.toSet ++ newLb.nonground)
            temp.normalizeSub(tpe, t)
          for(tpe <- newUb.ground.toSet ++ newUb.nonground)
            temp.normalizeSub(t, tpe)
        }
        bounds = temp.bounds
        unsat ++= temp.unsat
        _solution ++= sol
        sol = solveOnce
      }
    }

    private[CSet] def solveOnce: TSubst = {
      var sol: TSubst = Map()
      for ((tv, (lower, upper)) <- bounds) {
        if (isBipolar(tv)) {
          if (lower.isGround && upper.isGround)
            sol += tv -> lower.ground.get
        }
        else if (isProperPositive(tv)) {
          if (lower.isGround)
            sol += tv -> lower.ground.get
        }
        else if (isProperNegative(tv)) {
          if(upper.isGround)
            sol += tv -> upper.ground.get
        }
      }
      sol
    }

    private[CSet] def gameOver(c: Constraint) = unsat += c

    private[CSet] def addLowerBound(v: Symbol, t: Type) = {
      val (lower, upper) = bounds(v)
      val (newLower, error) = lower.add(t)
      if (error.nonEmpty)
        gameOver(Join(UVar(v), error))
      bounds += v -> (newLower, upper)
      val changed = if (t.isGround) newLower.ground.get else t
      for(t2 <- upper.ground.toSet ++ upper.nonground)
        normalizeSub(changed, t2)
    }

    private[CSet] def addUpperBound(v: Symbol, t: Type) = {
      val (lower, upper) = bounds(v)
      val (newUpper, error) = upper.add(t)
      if (error.nonEmpty)
        gameOver(Meet(UVar(v), error))
      bounds += v -> (lower, newUpper)
      val changed = if (t.isGround) newUpper.ground.get else t
      var subst: TSubst = Map()
      for(t2 <- lower.ground.toSet ++ lower.nonground)
        normalizeSub(t2, changed)
    }

    private[CSet] def mergeSubsts(sigma: TSubst, tau: TSubst): TSubst = {
      for ((v, t1) <- sigma if tau.isDefinedAt(v) && t1 != tau(v))
        gameOver(Equal(t1, tau(v)))
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
