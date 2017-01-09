package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava.{Type, _}
import Condition.trueCond
import constraints.CVar
import incremental.Util
import incremental.fjava.{CName, UCName}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable

trait CReq[T <: CReq[T]] {
  def self: T
  val cls: Type
  def withCls(t: Type): T = {
    val newcond = ConditionNested(cond.notGround, cond.notVar, cond.sameGroundAlternatives, cond.sameVar)
    if (ConditionNested.trueCond == newcond)
      withCls(t, Condition(Set(), Set(), Set(), Set(), cond.othersGround, cond.othersVar))
    else if (cls.isGround)
      withCls(t, Condition(Set(), Set(), Set(), Set(), cond.othersGround + (cls.asInstanceOf[CName] -> newcond), cond.othersVar))
    else
      withCls(t, Condition(Set(), Set(), Set(), Set(), cond.othersGround, cond.othersVar + (cls.asInstanceOf[UCName] -> newcond)))
  }
  def withCls(t: Type, newcond: Condition): T
  val cond: Condition
  def canMerge(other: CReq[T]): Boolean
  def assert(sat: CReq[T]): Option[Constraint] = alsoSame(sat.cls).flatMap(c2 => this.assert(sat, c2.cond))
  def assert(other: CReq[T], cond: Condition): Option[Constraint]
  def subst(s: CSubst): Option[T]

  def withCond(cond: Condition): T
  def alsoNot(n: Type): Option[T] = cond.alsoNot(cls, n) map (withCond(_))
  def alsoSame(n: Type): Option[T] = cond.alsoSame(cls, n) map (withCond(_))

  def satisfyExt(cls: CName, sup: CName): Seq[T] = {
    // NOTE: we assume that the removal of an extends clause implies no further declarations appear for subclass `ext.cls`

    // ext.cls != req.cls, so keep the original requirement
    val diff = this.alsoNot(cls)
    // ext.cls == req.cls, so replace the original requirement with a super-class requirement `ext.ext`
    val same = this.alsoSame(cls).map(_.withCls(sup))

    if (diff.isEmpty) {
      if (same.isEmpty)
        Seq()
      else
        Seq(same.get)
    }
    else {
      if (same.isEmpty)
        Seq(diff.get)
      else
        Seq(diff.get, same.get)
    }
  }
}
case class ExtCReq(cls: Type, ext: Type, cond: Condition = trueCond) extends CReq[ExtCReq] {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[ExtCReq]) = true
  def assert(other: CReq[ExtCReq], cond: Condition) = if (ext == other.self.ext) None else Some(Conditional(Equal(ext, other.self.ext), cls, cond))
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
  def assert(other: CReq[CtorCReq], cond: Condition) = if (args == other.self.args) None else Some(Conditional(AllEqual(args, other.self.args), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (CtorCReq(cls_, args.map(_.subst(s)), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
trait Named {
  val name: Symbol
}
case class FieldCReq(cls: Type, name: Symbol, typ: Type, cond: Condition = trueCond) extends CReq[FieldCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[FieldCReq]): Boolean = name == other.self.name
  def assert(other: CReq[FieldCReq], cond: Condition) = if (typ == other.self.typ) None else Some(Conditional(Equal(typ, other.self.typ), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (FieldCReq(cls_, name, typ.subst(s), _))
  }
  def withCond(c: Condition) = copy(cond = c)
}
case class MethodCReq(cls: Type, name: Symbol, params: Seq[Type], ret: Type, optionallyDefined: Boolean = false, cond: Condition = trueCond) extends CReq[MethodCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: Condition) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[MethodCReq]): Boolean = name == other.self.name
  def assert(other: CReq[MethodCReq], cond: Condition) = if (ret == other.self.ret && params == other.self.params) None else Some(Conditional(AllEqual(ret +: params, other.self.ret +: other.self.params), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (MethodCReq(cls_, name, params.map(_.subst(s)), ret.subst(s), optionallyDefined, _))
  }
  def uvars = cls.uvars ++ params.flatMap(_.uvars) ++ cond.uvars
  def withCond(c: Condition) = copy(cond = c)
}



case class ClassReqs (
                       currentCls: Option[Type] = None,
                       exts: Set[ExtCReq] = Set(),
                       ctors: Set[CtorCReq] = Set(),
                       fields: Map[Symbol, Set[FieldCReq]] = Map(),
                       methods: Map[Symbol, Set[MethodCReq]] = Map(),
                       optMethods: Map[Symbol, Set[MethodCReq]] = Map()) {

  override def toString =
    s"ClassReqs(current=$currentCls, ext=$exts, ctorParams=$ctors, fields=$fields, methods=$methods, optMethods=$optMethods)"

  def subst(s: CSubst): (ClassReqs, Seq[Constraint]) = {
    val currentX = currentCls.map(_.subst(s))
    val extX = exts.flatMap(_.subst(s))
    val ctorX = ctors.flatMap(_.subst(s))
    val (fieldX, cons1) = substMap(s, fields)
    val (methodX, cons2) = substMap(s, methods)
    val (optMethodX, cons3) = substMap(s, optMethods)
    val crs = ClassReqs(currentX, extX, ctorX, fieldX, methodX, optMethodX)
    (crs, cons1 ++ cons2 ++ cons3)
  }

  def substMap[T <: CReq[T] with Named](s: CSubst, m: Map[Symbol, Set[T]]): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    var res = Map[Symbol, Set[T]]()
    var cons = Seq[Constraint]()

    m.foreach { case (sym, set) =>
      val builder = new MapRequirementsBuilder[T]
      for (req <- set;
           sreq <- req.subst(s))
        builder.addReq(sreq)
      res += sym -> builder.getRequirements
      cons ++= builder.getConstraints
    }

    (res, cons)
  }

  def isEmpty =
    currentCls.isEmpty &&
    exts.isEmpty &&
    ctors.isEmpty &&
    fields.values.forall(_.isEmpty) &&
    methods.values.forall(_.isEmpty)

  def merge(crs: ClassReqs): (ClassReqs, Seq[Constraint]) = {
    val (currentX, cons0) = (currentCls.orElse(crs.currentCls), for (t1 <- currentCls; t2 <- crs.currentCls) yield Equal(t1, t2))
    val (extX, cons1) = merge(exts, crs.exts)
    val (ctorX, cons2) = merge(ctors, crs.ctors)
    val (fieldsX, cons3) = mergeMap(fields, crs.fields)
    val (methodsX, cons4) = mergeMap(methods, crs.methods)
    val (optMethodsX, cons5) = mergeMap(optMethods, crs.optMethods)
    val cons = cons0.toSeq ++ cons1 ++ cons2 ++ cons3 ++ cons4 ++ cons5
    (ClassReqs(currentX, extX, ctorX, fieldsX, methodsX, optMethodsX), cons)
  }

  private def merge[T <: CReq[T]](crs1: Set[T], crs2: Set[T]): (Set[T], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())

    Util.loop[Set[T], T, Constraint](addRequirement)(crs1, crs2)
  }

  private def mergeMap[T <: CReq[T] with Named](crs1: Map[Symbol, Set[T]], crs2: Map[Symbol, Set[T]]): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())

    Util.loop[Map[Symbol, Set[T]], T, Constraint](addRequirementMap)(crs1, crs2.values.toStream.flatten)
  }

  def addRequirement[T <: CReq[T]](crs: Set[T], req: T): (Set[T], Seq[Constraint]) = {
    // the new requirement, refined to be different from all existing requirements
    var diffreq: Option[T] = if (crs.size != 1) Some(req) else None
    // new constraints
    var cons = Seq[Constraint]()
    // updated requirements
    val diffcres = crs.flatMap{ creq =>
      if (creq.canMerge(req)) {
        diffreq = diffreq.flatMap(_.alsoNot(creq.cls))

        val diff1 = creq.alsoNot(req.cls)
        val same1 = creq.alsoSame(req.cls)
        val req1 = if (diff1.isEmpty) same1 else if (same1.isEmpty) diff1 else Some(creq)

        val diff2 = req.alsoNot(creq.cls)
        val same2 = req.alsoSame(creq.cls)
        val req2 = if (diff2.isEmpty) same2 else if (same2.isEmpty) diff2 else Some(req)

        val res = Seq() ++ req1 ++ req2

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

  def addRequirementMap[T <: CReq[T] with Named](crs: Map[Symbol, Set[T]], req: T): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    val set = crs.getOrElse(req.name,
      return (crs + (req.name -> Set(req)), Seq()))

    // the new requirement, refined to be different from all existing requirements
    var diffreq: Option[T] = if (set.size != 1) Some(req) else None

    val builder = new MapRequirementsBuilder[T]

    set.foreach{ creq =>
      diffreq = diffreq.flatMap(_.alsoNot(creq.cls))

      val diff1 = creq.alsoNot(req.cls)
      val same1 = creq.alsoSame(req.cls)
      val req1 = if (diff1.isEmpty) same1 else if (same1.isEmpty) diff1 else Some(creq)

      val diff2 = req.alsoNot(creq.cls)
      val same2 = req.alsoSame(creq.cls)
      val req2 = if (diff2.isEmpty) same2 else if (same2.isEmpty) diff2 else Some(req)

      if (!req1.isEmpty)
        builder.addReq(req1.get)
      if (!req2.isEmpty)
        builder.addReq(req2.get)
    }

    if (!diffreq.isEmpty)
      builder.addReq(diffreq.get)

    (crs + (req.name -> builder.getRequirements), builder.getConstraints)
  }


  def satisfyCReq[T <: CReq[T]](sat: T, crs: Set[T]): (Set[T], Seq[Constraint]) = {
    var cons = Seq[Constraint]()
    val newcrs = crs flatMap ( creq =>
      if (sat.canMerge(creq)) {
        val mcons = creq.assert(sat)
        if (!mcons.isEmpty)
          cons +:= mcons.get
        creq.alsoNot(sat.cls)
      }
      else
        Some(creq)
      )
    (newcrs, cons)
  }

  def satisfyCReqMap[T <: CReq[T] with Named](sat: T, crs: Map[Symbol, Set[T]]): (Map[Symbol, Set[T]], Seq[Constraint]) = {
    val set = crs.getOrElse(sat.name,
      return (crs, Seq()))

    var cons = Seq[Constraint]()
    val newcrs = set.flatMap( creq =>
      if (sat.canMerge(creq)) {
        cons ++= creq.assert(sat)
        creq.alsoNot(sat.cls)
      }
      else
        Some(creq)
    )
    if (newcrs.isEmpty)
      (crs - sat.name, cons)
    else
      (crs + (sat.name -> newcrs), cons)
  }
}

class MapRequirementsBuilder[T <: CReq[T] with Named] {
  private var newreqs = Map[Condition.MergeKey, T]()
  private var cons = Seq[Constraint]()
  def addReq(req: T) = {
    val key = req.cond.mergeKey(req.cls)
    newreqs.get(key) match {
      case None => newreqs += (key -> req)
      case Some(oreq) => {
        val reqCond = req.cond
        val oreqCond = oreq.cond
        if (key.needsMerge(reqCond, oreqCond)) {
          val mergeCond = key.merge(reqCond, oreqCond)
          newreqs += (key -> oreq.withCond(mergeCond))
          val c = req.assert(oreq, mergeCond)
          if (!c.isEmpty)
            cons +:= c.get
        }
        else if (req != oreq) {
          val c = req.assert(oreq, req.cond)
          if (!c.isEmpty)
            cons +:= c.get
        }
      }
    }
  }

  def getRequirements: Set[T] = newreqs.values.toSet
  def getConstraints: Seq[Constraint] = cons
}