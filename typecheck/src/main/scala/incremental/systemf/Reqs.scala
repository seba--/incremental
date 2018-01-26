package incremeantal.systemf

import Condition.trueCond
import constraints.Statistics
import constraints.equality._
import constraints.equality.CSubst.CSubst
import incremental.Node._
import incremental.{Node_, Util}
import incremental.systemf._

trait Req[T] {
  def self : T
  val varS : Symbol
  val varTyp :Type
  val cond: Condition
  def canMerge(other: Req[T]): Boolean
  def subst(s: CSubst): Option[T]

  def withCond(cond: Condition): T
  def alsoNot(n: Type): Option[T] = cond.alsoNot(varTyp, n) map (withCond(_))
  def lift: Reqs
}

case class VarReq(varS: Symbol, varTyp: Type, cond: Condition = trueCond) extends Req[VarReq] {
  def self = this
  def canMerge(other: Req[VarReq]): Boolean = varS == other.self.varS
  def subst(s: CSubst) = {
    val typ_ = varTyp.subst(s)
    cond.subst(varS, typ_, s) map (VarReq(varS, typ_, _))
  }
  def lift = Reqs(reqs = Set(this))
  def withCond(c: Condition) = copy(cond = c)
}

object Condition {
  val trueCond = Condition(Set())
}
case class Condition(not: Set[Type]){
  def subst(inst: Symbol, typ : Type, s: CSubst): Option[Condition] = {
    val newTyp = typ.subst(s)
    val newnot = not flatMap { n =>
      val n2 = n.subst(s)
      if (newTyp == n2)
        return None
      else if (newTyp.isGround && n2.isGround) // && cls != n2 (implicit)
        None
      else
        Some(n2)
    }
    Some(Condition(newnot))
  }

  def alsoNot(inst: Type, n: Type): Option[Condition] =
    if (inst == n )
      None
    else
      Some(Condition(not + n))

  def isGround: Boolean =
    not.foldLeft(true)((res, t) => res && t.isGround)
}

case class Reqs (
                       reqs: Set[VarReq] = Set()) {

  override def toString =
    s"VarReqs(Reqs$reqs)"

  def subst(s: CSubst): Reqs = Reqs(
    subst(reqs, s))

  private def subst[T <: Req[T]](crs: Set[T], s: CSubst): Set[T] = crs.flatMap (_.subst(s))

  def isEmpty = reqs.isEmpty


  def merge(sReq: Reqs): (Reqs, Seq[Constraint]) = {
    val (resreq, cons) = merge(reqs, sReq.reqs)
    (Reqs(resreq), cons)
  }

  private def merge[T <: Req[T]](reqs1: Set[T], reqs2: Set[T]): (Set[T], Seq[Constraint]) = {
    if (reqs1.isEmpty)
      return (reqs2, Seq())
    if (reqs2.isEmpty)
      return (reqs1, Seq())
    var cons = Seq[Constraint]()
    val cr = reqs1.flatMap ( cr1 =>
      reqs2.flatMap ( cr2 =>
        if (cr1.canMerge(cr2)) {
          val reqDiff = cr2.withCond(Condition(cr1.cond.not ++ cr1.cond.not))
          cons = cons :+ EqConstraint(cr2.varTyp, cr1.varTyp)
          Seq(reqDiff)
        }
        else
          Seq(cr1, cr2)
      )
    )
    (cr, cons)
  }

  def satisfyReq(req1: VarReq, setReqs: Set[VarReq], make: Set[VarReq] => Reqs): Reqs = {
    val newcrs = setReqs flatMap ( req2 =>
      if (req1.canMerge(req2)) {
        req2.alsoNot(req1.varTyp)
      }
      else
        Some(req2)
      )
    (make(newcrs))
  }

  def removeReq(req1: VarReq, setReqs: Set[VarReq], make: Set[VarReq] => Reqs): (Reqs, Seq[Constraint]) = {
    var cons = Seq[Constraint]()
    val newcrs = setReqs flatMap ( req2 =>
      if (req1.canMerge(req2)) {
        cons = cons :+ EqConstraint(req1.varTyp, req2.varTyp)
        Some(req2.withCond(Condition(Set())))
      }
      else
        Some(req2)
      )
    (make(newcrs), cons)
  }
}

