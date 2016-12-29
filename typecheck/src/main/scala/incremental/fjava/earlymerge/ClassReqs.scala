package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava._
import Condition.trueCond
import incremental.Util
import incremental.fjava.CName

import scala.collection.generic.CanBuildFrom

trait CReq[T <: CReq[T]] {
  def self: T
  val cls: Type
  def withCls(t: Type): T = withCls(t, Condition(Set(), Set(), cond.others + (cls -> Condition(cond.not, cond.same, Map()))))
  def withCls(t: Type, newcond: Condition): T
  val cond: Condition
  def canMerge(other: CReq[T]): Boolean
  def assert(other: CReq[T]): Option[Constraint] = for (c2 <- alsoSame(other.cls); c3 <- c2.alsoCond(other.cond)) yield assert(other, c3.cond)
  def assert(other: CReq[T], cond: Condition): Constraint
  def subst(s: CSubst): Option[T]

  def withCond(cond: Condition): T
  def alsoNot(n: Type): Option[T] = cond.alsoNot(cls, n) map (withCond(_))
  def alsoSame(n: Type): Option[T] = cond.alsoSame(cls, n) map (withCond(_))
  def alsoCond(other: Condition): Option[T] = cond.alsoCond(cls, other) map (withCond(_))

  def satisfyExt(ext: ExtCReq): Seq[T] = {
    // NOTE: we assume that the removal of an extends clause implies no further declarations appear for subclass `ext.cls`

    // ext.cls != req.cls, so keep the original requirement
    val diff = this.alsoNot(ext.cls)
    // ext.cls == req.cls, so replace the original requirement with a super-class requirement `ext.ext`
    val same = this.alsoSame(ext.cls).map(_.withCls(ext.ext))
    Seq(diff, same).flatten
  }
}
case class ExtCReq(cls: Type, ext: Type, cond: Condition = trueCond) extends CReq[ExtCReq] {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[ExtCReq]) = true
  def assert(other: CReq[ExtCReq], cond: Condition) = Conditional(Equal(ext, other.self.ext), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (ExtCReq(cls_, ext.subst(s), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
case class CtorCReq(cls: Type, args: Seq[Type], cond: Condition = trueCond) extends CReq[CtorCReq] {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[CtorCReq]) = true
  def assert(other: CReq[CtorCReq], cond: Condition) = Conditional(AllEqual(args, other.self.args), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (CtorCReq(cls_, args.map(_.subst(s)), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
case class FieldCReq(cls: Type, field: Symbol, typ: Type, cond: Condition = trueCond) extends CReq[FieldCReq] {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[FieldCReq]): Boolean = field == other.self.field
  def assert(other: CReq[FieldCReq], cond: Condition) = Conditional(Equal(typ, other.self.typ), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (FieldCReq(cls_, field, typ.subst(s), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
case class MethodCReq(cls: Type, name: Symbol, params: Seq[Type], ret: Type, optionallyDefined: Boolean = false, cond: Condition = trueCond) extends CReq[MethodCReq] {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[MethodCReq]): Boolean = name == other.self.name
  def assert(other: CReq[MethodCReq], cond: Condition) = Conditional(AllEqual(params :+ ret, other.self.params :+ other.self.ret), cls, cond)
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (MethodCReq(cls_, name, params.map(_.subst(s)), ret.subst(s), optionallyDefined, _))
  }
  def withCond(c: Condition) = copy(cond = c)
}

object Condition {
  val trueCond = new Condition(Set(), Set(), Map()) {
    override def subst(cls: Type, s: CSubst): Option[Condition] = Some(this)
    override def isGround: Boolean = true
    override def toString: String = "trueCond"
  }
}
case class Condition(not: Set[Type], same: Set[Type], others: Map[Type, Condition]){
  def subst(cls: Type, s: CSubst): Option[Condition] = {
    val newnot = not flatMap { n =>
      val n2 = n.subst(s)
      if (cls == n2)
        return None
      else if (cls.isGround && n2.isGround) // && cls != n2 (implicit)
        None
      else
        Some(n2)
    }
    val newsame = same flatMap { n =>
      val n2 = n.subst(s)
      if (cls == n2)
        None
      else if (cls.isGround && n2.isGround) // && cls != n2 (implicit)
        return None
      else
        Some(n2)
    }
    val newothers = others map { kv =>
      val cls = kv._1.subst(s)
      kv._2.subst(cls, s) match {
        case Some(cond) => cls -> cond
        case None => return None
      }
    }
    Some(Condition(newnot, newsame, newothers))
  }

  def alsoNot(cls: Type, n: Type): Option[Condition] =
    if (cls == n || same.contains(n))
      None
    else if (cls.isGround && n.isGround)
      Some(this) // because cls != n
    else
      Some(Condition(not + n, same, others))
  def alsoSame(cls: Type, n: Type): Option[Condition] =
    if (cls == n)
      Some(this)
    else if (cls.isGround && n.isGround || not.contains(n))
      None
    else
      Some(Condition(not, same + n, others))
  def alsoCond(cls: Type, other: Condition): Option[Condition] =
    if (other.not.contains(cls) || other.not.exists(same.contains(_)) || not.exists(other.same.contains(_)))
      None
    else
      Some(Condition(not ++ other.not, same ++ other.same, others))

  def isGround: Boolean =
    not.forall(_.isGround) && same.forall(_.isGround) && others.forall(_._2.isGround)
}

class Conditional(cons: Constraint, cls: Type, cond: Condition) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    val cls_ = cls.subst(cs.substitution)
    cond.subst(cls_, cs.substitution) match {
      case None => cs // discard this constraint because condition is false
      case Some(cond_) if cond_.isGround => cons.solve(cs)
      case Some(cond_) => cs.notyet(new Conditional(cons.subst(cs.substitution), cls_, cond_))
    }
  }

  override def subst(s: CSubst): Constraint = {
    val cls_ = cls.subst(s)
    Conditional(cons.subst(s), cls_, cond.subst(cls_, s).getOrElse(Condition.trueCond))
  }

  override def toString: String = s"Conditional($cons, $cls, $cond)"
}
object Conditional {
  def apply(cons: Constraint, cls: Type, cond: Condition): Constraint =
    if (cond == Condition.trueCond)
      cons
    else
      new Conditional(cons, cls, cond)
}



case class ClassReqs (
                       currentCls: Option[Type] = None,
                       exts: Set[ExtCReq] = Set(),
                       ctors: Set[CtorCReq] = Set(),
                       fields: Set[FieldCReq] = Set(),
                       methods: Set[MethodCReq] = Set(),
                       optMethods: Set[MethodCReq] = Set()) {

  override def toString =
    s"ClassReqs(current=$currentCls, ext=$exts, ctorParams=$ctors, fields=$fields, methods=$methods, optMethods=$optMethods)"

  def subst(s: CSubst): ClassReqs = ClassReqs(
    currentCls.map(_.subst(s)),
    exts.flatMap(_.subst(s)),
    ctors.flatMap(_.subst(s)),
    fields.flatMap(_.subst(s)),
    methods.flatMap(_.subst(s)),
    optMethods.flatMap(_.subst(s)))

  def isEmpty = currentCls.isEmpty && exts.isEmpty && ctors.isEmpty && fields.isEmpty && methods.isEmpty

  def merge(crs: ClassReqs): (ClassReqs, Seq[Constraint]) = {
    val (currentX, cons0) = (currentCls.orElse(crs.currentCls), for (t1 <- currentCls; t2 <- crs.currentCls) yield Equal(t1, t2))
    val (extX, cons1) = merge(exts, crs.exts)
    val (ctorX, cons2) = merge(ctors, crs.ctors)
    val (fieldsX, cons3) = merge(fields, crs.fields)
    val (methodsX, cons4) = merge(methods, crs.methods)
    val (optMethodsX, cons5) = merge(optMethods, crs.optMethods)
    val cons = cons0.toSeq ++ cons1 ++ cons2 ++ cons3 ++ cons4 ++ cons5
    (ClassReqs(currentX, extX, ctorX, fieldsX, methodsX, optMethodsX), cons)
  }

  private def merge[T <: CReq[T]](crs1: Set[T], crs2: Set[T]): (Set[T], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())

    Util.loop[Set[T], T, Constraint](addRequirement)(crs1, crs2)
  }

  def addRequirement[T <: CReq[T]](crs: Set[T], req: T): (Set[T], Seq[Constraint]) = {
    // the new requirement, refined to be different from all existing requirements
    var diffreq: Option[T] = Some(req)
    // new constraints
    var cons = Seq[Constraint]()
    // updated requirements
    val diffcres = crs.flatMap{ creq =>
      if (creq.canMerge(req)) {
        diffreq = diffreq.flatMap(_.alsoNot(creq.cls))

        val diff1 = creq.alsoNot(req.cls)
        val diff2 = req.alsoNot(creq.cls)
        val same1 = creq.alsoSame(req.cls).flatMap(_.alsoCond(req.cond))
        val same2 = req.alsoSame(creq.cls).flatMap(_.alsoCond(creq.cond))
        val res = Seq(diff1, diff2, same1, same2).flatten

//        // if creq.cls == req.cls then creq.assert(req)
//        val c = None // same1.map(cr => creq.assert(req, cr.cond))
//        cons = cons ++ c

        res
      }
      else
        Seq(creq)
    }
    (diffcres ++ diffreq, cons)
  }

  def satisfyCReq[T <: CReq[T]](creq1: T, crs: Set[T]): (Set[T], Seq[Constraint]) = {
    var cons = Seq[Constraint]()
    val newcrs = crs flatMap ( creq2 =>
      if (creq1.canMerge(creq2)) {
        cons ++= creq1.assert(creq2)
        creq2.alsoNot(creq1.cls)
      }
      else
        Some(creq2)
      )
    (newcrs, cons)
  }
}
