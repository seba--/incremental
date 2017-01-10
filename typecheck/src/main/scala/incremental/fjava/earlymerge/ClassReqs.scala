package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava.{Type, _}
import Condition.trueCond
import constraints.CVar
import incremental.{IncHashedSet, Util}
import incremental.fjava.{CName, UCName}

import scala.collection.mutable.ListBuffer

trait CReq[T <: CReq[T]] {
  def self: T
  val cls: Type
  def withCls(t: Type, newcond: ConditionTrait): T
  val cond: ConditionTrait
  def canMerge(other: CReq[T]): Boolean
  def assert(sat: CReq[T]): Option[Constraint] = alsoSame(sat.cls.asInstanceOf[CName]).flatMap(c2 => this.assert(sat, c2.cond))
  def assert(other: CReq[T], cond: ConditionTrait): Option[Constraint]
  def subst(s: CSubst): Option[T]

  def withCond(cond: ConditionTrait): T

  def alsoNot(n: CName): Option[T] =
    if (cls.isGround) {
      if (cls == n)
        None
      else
        Some(self)
    }
    else
      cond.asInstanceOf[Condition].alsoNot(cls, n) map (withCond(_))

  def alsoNot(n: UCName): Option[T] = cond.asInstanceOf[Condition].alsoNot(cls, n) map (withCond(_))

  def alsoSame(n: CName): Option[T] =
    if (cls.isGround) {
      if (cls == n)
        Some(self)
      else
        None
    }
    else
      cond.asInstanceOf[Condition].alsoSame(cls, n) map (withCond(_))

  def alsoSame(n: UCName): Option[T] = cond.asInstanceOf[Condition].alsoSame(cls, n) map (withCond(_))

  def satisfyExt(sub: CName, sup: CName): Seq[T] = {
    // NOTE: we assume that the removal of an extends clause implies no further declarations appear for subclass `sub`

    // ext.cls != req.cls, so keep the original requirement
    val diff =
      if (cls.isGround) {
        if (cls == sub)
          None
        else
          Some(self)
      }
      else
        this.alsoNot(sub)

    // ext.cls == req.cls, so replace the original requirement with a super-class requirement `ext.ext`
    val same =
      if (cls.isGround) {
        if (cls == sub) {
          if (cond.isInstanceOf[Condition]) {
            val cond_ = cond.asInstanceOf[Condition]
            val subCond = Condition(cond_.notGround, cond_.notVar, cond_.sameGroundAlternatives, cond_.sameVar)
            val supCond = ConditionOther(sub, subCond)
            Some(this.withCls(sup, supCond))
          }
          else {
            // val cond_ = cond.asInstanceOf[ConditionOther]
            // val subCond = trueCond
            // val supCond = cond_.add(sub, subCond)
            Some(this.withCls(sup, cond))
          }
        }
        else { // since cls != sub
          None
        }
      }
      else {
        // Note: since cond.othersGround is only set in satisfyExt alongside a concrete cls and because
        // this.cls is not ground, cond.othersGround must be empty. Consequently, the rest of cond must be non-empty (otherwise would be trueCond).
        val cond_ = cond.asInstanceOf[Condition]
        val subCond0 = new Condition(cond_.notGround, cond_.notVar, cond_.sameGroundAlternatives, cond_.sameVar + cls.asInstanceOf[UCName])
        subCond0.forCls(sub).map { subCond =>
          val supCond = ConditionOther(sub, subCond)
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
case class ExtCReq(cls: Type, ext: Type, cond: ConditionTrait = trueCond) extends CReq[ExtCReq] {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[ExtCReq]) = true
  def assert(other: CReq[ExtCReq], cond: ConditionTrait) = if (ext == other.self.ext) None else Some(Conditional(Equal(ext, other.self.ext), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (ExtCReq(cls_, ext.subst(s), _))
  }
  def withCond(c: ConditionTrait) = copy(cond = c)
}
case class CtorCReq(cls: Type, args: Seq[Type], cond: ConditionTrait = trueCond) extends CReq[CtorCReq] {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[CtorCReq]) = true
  def assert(other: CReq[CtorCReq], cond: ConditionTrait) = if (args == other.self.args) None else Some(Conditional(AllEqual(args, other.self.args), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (CtorCReq(cls_, args.map(_.subst(s)), _))
  }
  def withCond(c: ConditionTrait) = copy(cond = c)
}
trait Named {
  val name: Symbol
}
case class FieldCReq(cls: Type, name: Symbol, typ: Type, cond: ConditionTrait = trueCond) extends CReq[FieldCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[FieldCReq]): Boolean = name == other.self.name
  def assert(other: CReq[FieldCReq], cond: ConditionTrait) = if (typ == other.self.typ) None else Some(Conditional(Equal(typ, other.self.typ), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (FieldCReq(cls_, name, typ.subst(s), _))
  }
  def withCond(c: ConditionTrait) = copy(cond = c)
}
case class MethodCReq(cls: Type, name: Symbol, params: Seq[Type], ret: Type, optionallyDefined: Boolean = false, cond: ConditionTrait = trueCond) extends CReq[MethodCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[MethodCReq]): Boolean = name == other.self.name
  def assert(other: CReq[MethodCReq], cond: ConditionTrait) = if (ret == other.self.ret && params == other.self.params) None else Some(Conditional(AllEqual(ret +: params, other.self.ret +: other.self.params), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (MethodCReq(cls_, name, params.map(_.subst(s)), ret.subst(s), optionallyDefined, _))
  }
  def uvars = cls.uvars ++ params.flatMap(_.uvars) ++ cond.uvars
  def withCond(c: ConditionTrait) = copy(cond = c)
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
    if (crs.isEmpty)
      return (Seq(req), Seq.empty)

    val builder = new MapRequirementsBuilder[T]

    // updated requirements
    crs.foreach{ creq =>
      if (creq.cls.isGround && req.cls.isGround) {
        // if (creq.cls == req.cls) diff1 = None, diff2 = None, same1 = Some(creq), same2 = Some(req)
        // else                     diff1 = Some(creq), diff2 = Some(req), same1 = None, same2 = None
        // thus in all cases:
         builder.addReq(creq)
         builder.addReq(req)
      }
      else if (creq.cls.isGround) {
        // creq.cls is ground but req.cls is not ground (due to previous conditional branch)
        // thus req.cls is compatible with creq as either same or different
        // val diff1 = creq.alsoNot(req.cls)  =>  Some(...)
        // val same1 = creq.alsoSame(req.cls)  =>  Some(...)
        builder.addReq(creq)
        val diff2 = req.alsoNot(creq.cls.asInstanceOf[CName])
        val same2 = req.alsoSame(creq.cls.asInstanceOf[CName])
        val req2 = if (diff2.isEmpty) same2 else if (same2.isEmpty) diff2 else Some(req)
        if (!req2.isEmpty)
          builder.addReq(req2.get)
      }
      else if (req.cls.isGround) {
        // analogous to previous branch
        val diff1 = creq.alsoNot(req.cls.asInstanceOf[CName])
        val same1 = creq.alsoSame(req.cls.asInstanceOf[CName])
        val req1 = if (diff1.isEmpty) same1 else if (same1.isEmpty) diff1 else Some(creq)
        if (!req1.isEmpty)
          builder.addReq(req1.get)
        builder.addReq(req)
      }
      else {
        val diff1 = creq.alsoNot(req.cls.asInstanceOf[UCName])
        val same1 = creq.alsoSame(req.cls.asInstanceOf[UCName])
        val req1 = if (diff1.isEmpty) same1 else if (same1.isEmpty) diff1 else Some(creq)

        val diff2 = req.alsoNot(creq.cls.asInstanceOf[UCName])
        val same2 = req.alsoSame(creq.cls.asInstanceOf[UCName])
        val req2 = if (diff2.isEmpty) same2 else if (same2.isEmpty) diff2 else Some(req)

        if (!req1.isEmpty)
          builder.addReq(req1.get)
        if (!req2.isEmpty)
          builder.addReq(req2.get)
      }
    }

    (builder.getRequirements, builder.getConstraints)
  }

  def addRequirementMap[T <: CReq[T] with Named](crs: Map[Symbol, Seq[T]], req: T): (Map[Symbol, Seq[T]], Seq[Constraint]) = {
    val seq = crs.getOrElse(req.name,
      return (crs + (req.name -> Seq(req)), Seq()))

    val (newReqs, cons) = addRequirement(seq,req)
    (crs + (req.name -> newReqs), cons)
  }


  def satisfyCReq[T <: CReq[T]](sat: T, crs: Seq[T]): (Seq[T], Seq[Constraint]) = {
    // sat.cls is ground and sat.cond is trueCond
    var cons = Seq[Constraint]()
    val newcrs = crs flatMap ( creq =>
      if (sat.canMerge(creq)) {
        val mcons = creq.assert(sat)
        if (!mcons.isEmpty)
          cons +:= mcons.get
        creq.alsoNot(sat.cls.asInstanceOf[CName])
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
        creq.alsoNot(sat.cls.asInstanceOf[CName])
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
  private var newreqs = Map[(Type, MergeKey), ListBuffer[T]]()
  private var cons = Seq[Constraint]()
  def addReq(req: T) = {
    val mkey = req.cond.mergeKey
    val key = (req.cls, mkey)
    newreqs.get(key) match {
      case None =>
        val buf = ListBuffer(req)
        lists +:= buf
        newreqs += (key -> buf)
      case Some(oreqs) => {
        val oreq = oreqs(0)
        val oreqCond = oreq.cond
        if (mkey.needsMerge(oreqCond)) {
          oreqs += req
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
      val head = buf(0)
      val headcond = head.cond

      val mergeCond = if (headcond.isInstanceOf[Condition]) {
        val headcond_ = headcond.asInstanceOf[Condition]
        var sameGroundAlternatives = IncHashedSet.empty[CName]
        buf.foreach { c =>
          val cond = c.cond.asInstanceOf[Condition]
          sameGroundAlternatives ++= cond.sameGroundAlternatives
        }
        Condition(
          headcond_.notGround,
          headcond_.notVar,
          sameGroundAlternatives,
          headcond_.sameVar
        )
      }
      else {
        val headcond_ = headcond.asInstanceOf[ConditionOther]
        var othersGroundSameGround = IncHashedSet.empty[CName]
        buf.foreach { c =>
          val c_ = c.cond.asInstanceOf[ConditionOther]
          othersGroundSameGround ++= c_.cond.sameGroundAlternatives
        }
        val other = Condition(headcond_.cond.notGround, headcond_.cond.notVar, othersGroundSameGround, headcond_.cond.sameVar)
        new ConditionOther(headcond_.cls, other)
      }

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