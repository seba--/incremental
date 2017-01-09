package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava.{Type, _}
import Condition.trueCond
import constraints.CVar
import incremental.Util
import incremental.fjava.{CName, UCName}

import scala.collection.mutable.ListBuffer

trait CReq[T <: CReq[T]] {
  def self: T
  val cls: Type
  def withCls(t: Type, newcond: Condition): T
  val cond: Condition
  def canMerge(other: CReq[T]): Boolean
  def assert(sat: CReq[T]): Option[Constraint] = alsoSame(sat.cls).flatMap(c2 => this.assert(sat, c2.cond))
  def assert(other: CReq[T], cond: Condition): Option[Constraint]
  def subst(s: CSubst): Option[T]

  def withCond(cond: Condition): T
  def alsoNot(n: Type): Option[T] = cond.alsoNot(cls, n) map (withCond(_))
  def alsoSame(n: Type): Option[T] = cond.alsoSame(cls, n) map (withCond(_))

  def satisfyExt(sub: CName, sup: CName): Seq[T] = {
    // NOTE: we assume that the removal of an extends clause implies no further declarations appear for subclass `ext.cls`

    // ext.cls != req.cls, so keep the original requirement
    val diff = this.alsoNot(sub)
    // ext.cls == req.cls, so replace the original requirement with a super-class requirement `ext.ext`
    val same = this.alsoSame(sub).map { _ =>
      if (cls.isGround) {
        if (cls == sub)
          self
        else {
          val subCond = new ConditionNested(cond.notGround, cond.notVar, cond.sameGroundAlternatives + cls.asInstanceOf[CName], cond.sameVar)
          val supCond = Condition(Set(), Set(), Set(), Set(), cond.othersGround + (sub -> subCond))
          this.withCls(sup, supCond)
        }
      }
      else {
        val subCond = new ConditionNested(cond.notGround, cond.notVar, cond.sameGroundAlternatives, cond.sameVar + cls.asInstanceOf[UCName])
        val supCond = Condition(Set(), Set(), Set(), Set(), cond.othersGround + (sub -> subCond))
        this.withCls(sup, supCond)
      }
    }

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
                       exts: Seq[ExtCReq] = Seq(),
                       ctorsGround: Seq[CtorCReq] = Seq(),
                       ctorsVar: Seq[CtorCReq] = Seq(),
                       fields: Map[Symbol, Seq[FieldCReq]] = Map(),
                       methods: Map[Symbol, Seq[MethodCReq]] = Map(),
                       optMethods: Map[Symbol, Seq[MethodCReq]] = Map()) {

  override def toString =
    s"ClassReqs(current=$currentCls, ext=$exts, ctorParams=${ctorsGround ++ ctorsVar}, fields=$fields, methods=$methods, optMethods=$optMethods)"

  def subst(s: CSubst): (ClassReqs, Seq[Constraint]) = {
    val currentX = currentCls.map(_.subst(s))
    val extX = exts.flatMap(_.subst(s))
    val (ctorGroundX, ctorVarX, cons1) = substCtors(s, ctorsGround, ctorsVar)
    val (fieldX, cons2) = substMapSeq(s, fields)
    val (methodX, cons3) = substMapSeq(s, methods)
    val (optMethodX, cons4) = substMapSeq(s, optMethods)
    val crs = ClassReqs(currentX, extX, ctorGroundX, ctorVarX, fieldX, methodX, optMethodX)
    (crs, cons1 ++ cons2 ++ cons3 ++ cons4)
  }

  def substCtors(s: CSubst, ground: Seq[CtorCReq], vars: Seq[CtorCReq]): (Seq[CtorCReq], Seq[CtorCReq], Seq[Constraint]) = {
    val groundBuilder = new MapRequirementsBuilder[CtorCReq]
    val varsBuilder = new MapRequirementsBuilder[CtorCReq]

    ground.foreach { greq =>
      val mreq = greq.subst(s)
      if (!mreq.isEmpty)
        groundBuilder.addReq(mreq.get)
    }

    vars.foreach { vreq =>
      val mreq = vreq.subst(s)
      if (!mreq.isEmpty) {
        val req = mreq.get
        if (req.cls.isGround)
          groundBuilder.addReq(req)
        else
          varsBuilder.addReq(req)
      }
        None
    }

    val newGround = groundBuilder.getRequirements
    val newVars = varsBuilder.getRequirements
    (newGround, newVars, groundBuilder.getConstraints ++ varsBuilder.getConstraints)
  }

  def substMapSeq[T <: CReq[T] with Named](s: CSubst, m: Map[Symbol, Seq[T]]): (Map[Symbol, Seq[T]], Seq[Constraint]) = {
    var res = Map[Symbol, Seq[T]]()
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
    ctorsGround.isEmpty &&
    ctorsVar.isEmpty &&
    fields.values.forall(_.isEmpty) &&
    methods.values.forall(_.isEmpty)

  def merge(crs: ClassReqs): (ClassReqs, Seq[Constraint]) = {
    val (currentX, cons0) = (currentCls.orElse(crs.currentCls), for (t1 <- currentCls; t2 <- crs.currentCls) yield Equal(t1, t2))
    val (extX, cons1) = merge(exts, crs.exts)
    val (ctorGroundX, cons2g) = merge(ctorsGround, crs.ctorsGround)
    val (ctorVarX, cons2v) = merge(ctorsVar, crs.ctorsVar)
    val (fieldsX, cons3) = mergeMap(fields, crs.fields)
    val (methodsX, cons4) = mergeMap(methods, crs.methods)
    val (optMethodsX, cons5) = mergeMap(optMethods, crs.optMethods)
    val cons = cons0.toSeq ++ cons1 ++ cons2g ++ cons2v ++ cons3 ++ cons4 ++ cons5
    (ClassReqs(currentX, extX.distinct, ctorGroundX, ctorVarX, fieldsX, methodsX, optMethodsX), cons)
  }

  private def merge[T <: CReq[T]](crs1: Seq[T], crs2: Seq[T]): (Seq[T], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())

    Util.loop[Seq[T], T, Constraint](addRequirement)(crs1, crs2)
  }

  private def mergeMap[T <: CReq[T] with Named](crs1: Map[Symbol, Seq[T]], crs2: Map[Symbol, Seq[T]]): (Map[Symbol, Seq[T]], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())

    Util.loop[Map[Symbol, Seq[T]], T, Constraint](addRequirementMap)(crs1, crs2.values.toStream.flatten)
  }

  def addRequirement[T <: CReq[T]](crs: Seq[T], req: T): (Seq[T], Seq[Constraint]) = {
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

  def addRequirementMap[T <: CReq[T] with Named](crs: Map[Symbol, Seq[T]], req: T): (Map[Symbol, Seq[T]], Seq[Constraint]) = {
    val seq = crs.getOrElse(req.name,
      return (crs + (req.name -> Seq(req)), Seq()))

    // the new requirement, refined to be different from all existing requirements
    var diffreq: Option[T] = if (seq.size != 1) Some(req) else None

    val builder = new MapRequirementsBuilder[T]

    seq.foreach{ creq =>
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


  def satisfyCReq[T <: CReq[T]](sat: T, crs: Seq[T]): (Seq[T], Seq[Constraint]) = {
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

  def satisfyCReqMap[T <: CReq[T] with Named](sat: T, crs: Map[Symbol, Seq[T]]): (Map[Symbol, Seq[T]], Seq[Constraint]) = {
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

class MapRequirementsBuilder[T <: CReq[T]] {
  private var lists = Seq[ListBuffer[T]]()
  private var newreqs = Map[Condition.MergeKey, ListBuffer[T]]()
  private var cons = Seq[Constraint]()
  def addReq(req: T) = {
    val key = req.cond.mergeKey(req.cls)
    newreqs.get(key) match {
      case None =>
        val buf = ListBuffer(req)
        lists +:= buf
        newreqs += (key -> buf)
      case Some(oreqs) => {
        val reqCond = req.cond
        val oreq = oreqs(0)
        val oreqCond = oreq.cond
        if (key.needsMerge(reqCond, oreqCond)) {
//          val mergeCond = key.merge(reqCond, oreqCond)
          oreqs += req
//          val c = req.assert(oreq, mergeCond)
//          if (!c.isEmpty)
//            cons +:= c.get
        }
        else if (req != oreq) {
          val c = req.assert(oreq, req.cond)
          if (!c.isEmpty)
            cons +:= c.get
        }
      }
    }
  }

  def getRequirements: Seq[T] = {
    var seq = Seq[T]()
    lists.foreach { buf =>
      var sameGroundAlternatives = Set[CName]()
      var othersGroundSameGround = Map[CName, Set[CName]]()
      buf.foreach { c =>
        val cond = c.cond
        sameGroundAlternatives ++= cond.sameGroundAlternatives

        if (othersGroundSameGround.isEmpty)
          othersGroundSameGround = cond.othersGround.mapValues(_.sameGroundAlternatives)
        else
          othersGroundSameGround = othersGroundSameGround.map { case (key, cts) => key -> (cts ++ cond.othersGround(key).sameGroundAlternatives ) }
      }
      val head = buf(0)
      val headcond = head.cond
      val mergeCond = Condition(
        headcond.notGround,
        headcond.notVar,
        sameGroundAlternatives,
        headcond.sameVar,
        headcond.othersGround.map { case (key,cn) => key -> new ConditionNested(cn.notGround, cn.notVar, othersGroundSameGround(key), cn.sameVar) }
      )
      seq +:= head.withCond(mergeCond)
      buf.foreach{ req =>
        val c = req.assert(head, mergeCond)
        if (!c.isEmpty)
          cons +:= c.get
      }
    }
    seq
  }
  def getConstraints: Seq[Constraint] = cons
}