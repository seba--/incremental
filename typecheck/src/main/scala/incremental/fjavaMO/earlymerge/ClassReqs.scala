package incremental.fjavaMO.earlymerge

import constraints.fjavaMO.CSubst.CSubst
import constraints.fjavaMO.{Type, _}
import Condition.trueCond
import constraints.CVar
import incremental.{Cell, IncHashedSet, Util}
import incremental.fjavaMO.{CName, UCName}

import scala.collection.mutable.ListBuffer
import scala.ref.Reference

trait CReq[T <: CReq[T]] {
  def self: T
  val cls: Type
  def withCls(t: Type, newcond: ConditionTrait): T
  val cond: ConditionTrait
  val cons: Set[Constraint]
  def canMerge(other: CReq[T]): Boolean
  def assert(sat: CReq[T]): Seq[Constraint] =
    this.alsoSame(sat.cls.asInstanceOf[CName]) match {
      case
        None => Seq()
      case Some(same) => this.assert(sat, same.cond) match {
        case None => Seq()
        case Some(c) => {
          var res = Seq[Constraint]()
          cons.foreach { c =>
            res +:= Conditional(c, cls, same.cond)
          }
          c +: res
        }
      }
    }
  def assert(other: CReq[T], cond: ConditionTrait): Option[Constraint]
  def subst(s: CSubst): Option[T]
  def withCond(cond: ConditionTrait, cons: Set[Constraint]): T

  def alsoNot(n: CName): Option[T] =
    if (cls.isGround) {
      if (cls == n)
        None
      else
        Some(self)
    }
    else
      cond.asInstanceOf[Condition].alsoNot(cls, n) map (withCond(_, cons))

  def alsoNot(n: UCName): Option[T] = cond.asInstanceOf[Condition].alsoNot(cls, n) map (withCond(_, cons))

  def alsoSame(n: CName): Option[T] =
    if (cls.isGround) {
      if (cls == n)
        Some(self)
      else
        None
    }
    else
      cond.asInstanceOf[Condition].alsoSame(cls, n) map (withCond(_, cons))

  def alsoSame(n: UCName): Option[T] = cond.asInstanceOf[Condition].alsoSame(cls, n) map (withCond(_, cons))

  def satisfyExt(chain: Seq[(CName, CName)]): Seq[T] = {
    var res = Seq(self)
    for ((sub, sup) <- chain)
      res = res.flatMap(_.satisfyExt(sub, sup))
    res
  }

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
        val subCond0 = cond_.alsoSame(cls, sub)
        subCond0.map { subCond =>
          val supCond = ConditionOther(cls, subCond)
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
      else {
        Seq(diff.get, same.get) // TODO keep the two reqs together to improve merge behavior (yielding a single unconditional constraint)
      }
    }
  }
}

case class ExtCReq(cls: Type, ext: Type, cond: ConditionTrait = trueCond, cons: Set[Constraint] = Set()) extends CReq[ExtCReq] {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[ExtCReq]) = true
  def assert(other: CReq[ExtCReq], cond: ConditionTrait) = if (ext == other.self.ext) None else Some(Conditional(Equal(ext, other.self.ext), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (ExtCReq(cls_, ext.subst(s), _, cons.map(_.subst(s))))
  }
  def withCond(c: ConditionTrait, cons: Set[Constraint]) = copy(cond = c, cons = cons)
}
case class CtorCReq(cls: Type, args: Seq[Type], cond: ConditionTrait = trueCond, cons: Set[Constraint] = Set()) extends CReq[CtorCReq] {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[CtorCReq]) = true
  def assert(other: CReq[CtorCReq], cond: ConditionTrait) = if (args == other.self.args) None else Some(Conditional(AllEqual(args, other.self.args), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (CtorCReq(cls_, args.map(_.subst(s)), _, cons.map(_.subst(s))))
  }
  def withCond(c: ConditionTrait, cons: Set[Constraint]) = copy(cond = c, cons = cons)
}
trait Named {
  val name: Symbol
}
case class FieldCReq(cls: Type, name: Symbol, typ: Type, cond: ConditionTrait = trueCond, cons: Set[Constraint] = Set()) extends CReq[FieldCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[FieldCReq]): Boolean = name == other.self.name
  def assert(other: CReq[FieldCReq], cond: ConditionTrait) = if (typ == other.self.typ) None else Some(Conditional(Equal(typ, other.self.typ), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (FieldCReq(cls_, name, typ.subst(s), _, cons.map(_.subst(s))))
  }
  def withCond(c: ConditionTrait, cons: Set[Constraint]) = copy(cond = c, cons = cons)


  case class MethodCReq(cls: Type, name: Symbol, params: Seq[Type], ret: Type, minselSet: Map[Type, Set[Seq[Type]]], flagMinsel: Boolean = false, optionallyDefined: Boolean = false, cond: ConditionTrait = trueCond, cons: Set[Constraint] = Set()) extends CReq[MethodCReq] with Named {
  def self = this
  def withCls(t: Type, newcond: ConditionTrait) = copy(cls=t, cond=newcond)
  def canMerge(other: CReq[MethodCReq]): Boolean = name == other.self.name
  def assert(other: CReq[MethodCReq], cond: ConditionTrait) = if (ret == other.self.ret && params == other.self.params) None else Some(Conditional(AllEqual(ret +: params, other.self.ret +: other.self.params), cls, cond))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    val newcons = cons.map(_.subst(s))
    cond.subst(cls_, s) map (MethodCReq(cls_, name, params.map(_.subst(s)), ret.subst(s), Map(), flagMinsel, optionallyDefined, _, newcons))
  }
  def withCond(c: ConditionTrait, cons: Set[Constraint]) = {
    if (trueCond==c && cons.nonEmpty)
      println("WARNING withCond")
    copy(cond = c, cons = cons)
  }
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
    val optMethodX = substOptMapSeq(s, optMethods)
    val crs = ClassReqs(currentX, extX, ctorGroundX, ctorVarX, fieldX, methodX, optMethodX)
    (crs, cons1 ++ cons2 ++ cons3)
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

    m.foreach { case (sym, reqs) =>
      val builder = new MapRequirementsBuilder[T]
      for (req <- reqs;
           sreq <- req.subst(s))
        builder.addReq(sreq)
      res += sym -> builder.getRequirements
      cons ++= builder.getConstraints
    }

    (res, cons)
  }

  def substOptMapSeq[T <: CReq[T] with Named](s: CSubst, m: Map[Symbol, Seq[T]]): Map[Symbol, Seq[T]] =
    m.mapValues( reqs => reqs.flatMap(_.subst(s)) ).view.force


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
    val optMethodsX = mergeOptMap(optMethods, crs.optMethods)
    val cons = cons0.toSeq ++ cons1 ++ cons2g ++ cons2v ++ cons3 ++ cons4
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

    Util.loop[Map[Symbol, Seq[T]], T, Constraint](addRequirementMap)(crs1, crs2.toStream.map(_._2).flatten)
  }

  private def mergeOptMap[T <: CReq[T] with Named](crs1: Map[Symbol, Seq[T]], crs2: Map[Symbol, Seq[T]]): Map[Symbol, Seq[T]] = {
    if (crs1.isEmpty)
      return crs2

    crs2.toStream.map(_._2).flatten.foldLeft(crs1)((crs, req) => addRequirementOptMap(crs, req))
  }

  def addRequirement[T <: CReq[T]](crs: Seq[T], req: T): (Seq[T], Seq[Constraint]) = {
    if (crs.isEmpty)
      return (Seq(req), Seq.empty)

    val builder = new MapRequirementsBuilder[T]
    var addedReq = false

    // updated requirements
    crs.foreach{ creq =>
      if (creq.cls.isGround && req.cls.isGround) {
        // if (creq.cls == req.cls) diff1 = None, diff2 = None, same1 = Some(creq), same2 = Some(req)
        // else                     diff1 = Some(creq), diff2 = Some(req), same1 = None, same2 = None
        // thus in all cases:
        builder.addReq(creq)
        if (!addedReq) {
          builder.addReq(req)
          addedReq = true
        }
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
        if (!addedReq) {
          builder.addReq(req)
          addedReq = true
        }
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

    val (newReqs, cons) = addRequirement(seq, req)
    (crs + (req.name -> newReqs), cons)
  }

  def addRequirementOptMap[T <: CReq[T] with Named](crs: Map[Symbol, Seq[T]], req: T): Map[Symbol, Seq[T]] = {
    val seq = crs.getOrElse(req.name,
      return crs + (req.name -> Seq(req)))

    if (seq.contains(req))
      crs
    else
      crs + (req.name -> (req +: seq))
  }

  def satisfyCReq[T <: CReq[T]](sat: T, crs: Seq[T]): (Seq[T], Seq[Constraint]) = {
    // sat.cls is ground and sat.cond is trueCond
    var cons = Seq[Constraint]()
    val newcrs = crs flatMap ( creq =>
      if (sat.canMerge(creq)) {
        cons ++= creq.assert(sat)
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

object MapRequirementsBuilder {
  class Key(val t: Type, val mk: MergeKey) {
    override val hashCode: Int = t.hashCode + 31*mk.hashCode

    override def equals(obj: scala.Any): Boolean = obj match {
      case other: Key => t == other.t && mk == other.mk
      case _ => false
    }

  }
}

class MapRequirementsBuilder[T <: CReq[T]] {
  private var lists = ListBuffer[(T, Cell[Set[Constraint]], Cell[IncHashedSet[CName]])]()
  private var listsOther = ListBuffer[(T, Cell[Set[Constraint]], Cell[IncHashedSet[CName]])]()
  private var newreqs = Map[MapRequirementsBuilder.Key, (T, Cell[Set[Constraint]], Cell[IncHashedSet[CName]])]()
  private var newreqsOther = Map[MapRequirementsBuilder.Key, (T, Cell[Set[Constraint]], Cell[IncHashedSet[CName]])]()
  private var cons = Seq[Constraint]()
  def addReq(req: T) = {
    val mkey = req.cond.mergeKey
    val key = new MapRequirementsBuilder.Key(req.cls, mkey)
    val isOther = req.cond.isInstanceOf[ConditionOther]
    (if (!isOther) newreqs.get(key) else newreqsOther.get(key)) match {
      case None =>
        val trueCons = Cell(req.cons)
        val set =
          if (!isOther) req.cond.asInstanceOf[Condition].sameGroundAlternatives
          else req.cond.asInstanceOf[ConditionOther].cond.sameGroundAlternatives
        val refset = Cell(set)
        val v = (req, trueCons, refset)
        if (!isOther) {
          lists += v
          newreqs += (key -> v)
        }
        else {
          listsOther += v
          newreqsOther += (key -> v)
        }
      case Some((oreq, trueCons, set)) => {
        val oreqCond = oreq.cond
        if (mkey.needsMerge(oreqCond)) {
          if (!isOther) set.ref = set.ref ++ req.cond.asInstanceOf[Condition].sameGroundAlternatives
          else set.ref = set.ref ++ req.cond.asInstanceOf[ConditionOther].cond.sameGroundAlternatives
        }

        trueCons.ref = trueCons.ref ++ req.cons
        val c = req.assert(oreq, trueCond)
        if (!c.isEmpty)
          trueCons.ref = trueCons.ref + c.get
      }
    }
  }

  def getRequirements: Seq[T] = {
    var seq = Seq[T]()

    listsOther.foreach { case (oreq, trueCons, sameGroundAlternatives) =>
      val oreqCond = oreq.cond

      val headcond_ = oreqCond.asInstanceOf[ConditionOther]

      if (trueCons.ref.nonEmpty && !sameGroundAlternatives.ref.isEmpty && headcond_.cond.notGround.isEmpty && headcond_.cond.notVar.isEmpty && headcond_.cond.sameVar.isEmpty) {
        // try to find negated condition that entails the same constraints
        val negcond = new Condition(sameGroundAlternatives.ref, IncHashedSet.empty, IncHashedSet.empty, IncHashedSet.empty)
        val mkey = negcond.mergeKey
        val key = new MapRequirementsBuilder.Key(headcond_.cls, mkey)
        newreqs.get(key) match {
          case None =>
          case Some(((oreq2, trueCons2, sameGroundAlternatives2))) =>
            if (sameGroundAlternatives2.ref.isEmpty && trueCons == trueCons2) {
              cons ++= trueCons.ref
              trueCons.ref = Set()
              trueCons2.ref = Set()

            }
        }
      }

      val other = Condition(headcond_.cond.notGround, headcond_.cond.notVar, sameGroundAlternatives.ref, headcond_.cond.sameVar)
      val mergeCond = ConditionOther(headcond_.cls, other)

      if (trueCond == mergeCond) {
        seq +:= oreq.withCond(mergeCond, Set())
        cons ++= trueCons.ref
      }
      else
        seq +:= oreq.withCond(mergeCond, trueCons.ref)
    }

    lists.foreach { case (oreq, trueCons, sameGroundAlternatives) =>
      val oreqCond = oreq.cond
      val headcond_ = oreqCond.asInstanceOf[Condition]
      val mergeCond = Condition(headcond_.notGround, headcond_.notVar, sameGroundAlternatives.ref, headcond_.sameVar)

      if (trueCond == mergeCond) {
        seq +:= oreq.withCond(mergeCond, Set())
        cons ++= trueCons.ref
      }
      else {
        seq +:= oreq.withCond(mergeCond, trueCons.ref)
      }

//      trueCons.ref.foreach { c =>
//        cons +:= Conditional(c, oreq.cls, mergeCond)
//      }
    }
    seq
  }
  def getConstraints: Seq[Constraint] = cons
}