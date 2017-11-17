package incremental.fjavaMO.earlymerge

import constraints.fjavaMO.CSubst.CSubst
import constraints.fjavaMO.{Constraint, Equal, Type}
import incremental.Util
import incremental.fjavaMO.CName


sealed trait ClassFact
case class CtorFact(cls: CName, args: Seq[CName]) extends ClassFact
case class FieldFact(cls: CName, field: Symbol, typ: CName) extends ClassFact
case class MethodFact(cls: CName, name: Symbol, params: Seq[CName], ret: CName) extends ClassFact

/*
 * Invariant: All cfacts have been used to satisfy requirements in creqs.
 */
case class ClassContext(creqs: ClassReqs = ClassReqs(), cfacts: Seq[ClassFact] = Seq(), extFacts: Map[CName, CName] = Map()) {

  def subst(s: CSubst): (ClassContext, Seq[Constraint]) = {
    val (newcreqs, cons) = creqs.subst(s)
    (withCReqs(newcreqs), cons)
  }

  def finalized: ClassContext = withCReqs(creqs.copy(optMethods = Map()))

  def merge(other: ClassContext): (ClassContext, Seq[Constraint]) = {
    val (satisfiedThis, cons1) = this.addFacts(other.cfacts)
    val (satisfiedThisExt, cons2) = satisfiedThis.addExtFacts(other.extFacts)
    val (satisfiedOther, cons3) = other.addFacts(this.cfacts)
    val (satisfiedOtherExt, cons4) = satisfiedOther.addExtFacts(this.extFacts)
    val (mergedReqs, cons5) = satisfiedThisExt.creqs.merge(satisfiedOtherExt.creqs)
    val newcons = cons1 ++ cons2 ++ cons3 ++ cons4 ++ cons5
    (satisfiedThisExt.withCReqs(mergedReqs), newcons)
  }

  def addFact(fact: ClassFact): (ClassContext, Seq[Constraint]) = fact match {
    case CtorFact(cls, args) =>
      val (crsGround, cons1) = creqs.satisfyCReq(CtorCReq(cls, args), creqs.ctorsGround)
      val (crsVar, cons2) = creqs.satisfyCReq(CtorCReq(cls, args), creqs.ctorsGround)
      val cctx = ClassContext(creqs.copy(ctorsGround = crsGround, ctorsVar = crsVar), fact +: cfacts, extFacts)
      (cctx, cons1 ++ cons2)

    case FieldFact(cls, field, typ) =>
      val (crs, cons) = creqs.satisfyCReqMap(FieldCReq(cls, field, typ), creqs.fields)
      val cctx = ClassContext(creqs.copy(fields = crs), fact +: cfacts, extFacts)
      (cctx, cons)

    case MethodFact(cls, name, params, ret) =>
      val (crsMethods, cons1) = creqs.satisfyCReqMap(MethodCReq(cls, name, params, ret, Map(), Set() ), creqs.methods)
      val (crsOptMethods, cons2) = creqs.satisfyCReqMap(MethodCReq(cls, name, params, ret, Map(), Set()), creqs.optMethods)
      val cctx = ClassContext(creqs.copy(methods = crsMethods, optMethods = crsOptMethods), fact +: cfacts, extFacts)
      (cctx, cons1 ++ cons2)
  }

  def getExtendsChain(sub: CName, cls: CName): Seq[(CName, CName)] = {
    var top = cls
    var tail = Seq((sub, cls))
    while (true)
      extFacts.get(top) match {
        case None => return tail.reverse
        case Some(newtop) => {
          tail = (top, newtop) +: tail
          top = newtop
        }
      }
    ???
  }

  def addExtFact(cls: CName, sup: CName) = {
    var chain = getExtendsChain(cls, sup)

    val (crs, cons1) = Util.loop[Seq[ExtCReq], (CName, CName), Constraint]((exts, ext) => creqs.satisfyCReq(ExtCReq(ext._1, ext._2), exts))(creqs.exts, chain)
    val (newFields, cons2) = addExtFactMap(creqs.fields, chain)
    val (newMethods, cons3) = addExtFactMap(creqs.methods, chain)
    val (newOptMethods, cons4) = addExtFactMap(creqs.optMethods, chain)
    val cctx = ClassContext(creqs.copy(exts = crs, fields = newFields, methods = newMethods, optMethods = newOptMethods), cfacts, extFacts + (cls -> sup))
    (cctx, cons1 ++ cons2 ++ cons3 ++ cons4)
  }

  def addExtFactMap[T <: CReq[T] with Named](crs: Map[Symbol, Seq[T]], chain: Seq[(CName, CName)]): (Map[Symbol, Seq[T]], Seq[Constraint]) = {
    var cons = Seq[Constraint]()
    val newcrs = crs.mapValues { reqs =>
      val builder = new MapRequirementsBuilder[T]
      for (req <- reqs;
           newreq <- req.satisfyExt(chain)) {
        builder.addReq(newreq)
      }
      val res = builder.getRequirements
      cons ++= builder.getConstraints
      res
    }

    (newcrs.view.force, cons)
  }

  def addFacts(facts: Iterable[ClassFact]): (ClassContext, Seq[Constraint]) =
    Util.loop[ClassContext, ClassFact, Constraint](_.addFact(_))(this, facts)

  def addExtFacts(facts: Iterable[(CName, CName)]): (ClassContext, Seq[Constraint]) =
    Util.loop[ClassContext, (CName, CName), Constraint]((o, in) => o.addExtFact(in._1, in._2))(this, facts)

  def addRequirement(req: CReq[_]): (ClassContext, Seq[Constraint]) = req match {
    case req: CtorCReq =>
      if (req.cls.isGround) {
        val (crs, cons) = creqs.addRequirement(creqs.ctorsGround, req)
        (withCReqs(creqs.copy(ctorsGround = crs)), cons)
      }
      else {
        val (crs, cons) = creqs.addRequirement(creqs.ctorsGround, req)
        (withCReqs(creqs.copy(ctorsVar = crs)), cons)
      }

    case req: FieldCReq =>
      val (crs, cons) = creqs.addRequirementMap(creqs.fields, req)
      (withCReqs(creqs.copy(fields = crs)), cons)

    case req: MethodCReq =>
      if (!req.optionallyDefined) {
        val (crs, cons) = creqs.addRequirementMap(creqs.methods, req)
        (withCReqs(creqs.copy(methods = crs)), cons)
      }
      else {
        val (crs, cons) = creqs.addRequirementMap(creqs.optMethods, req)
        (withCReqs(creqs.copy(optMethods = crs)), cons)
      }

    case req: ExtCReq =>
      val (crs, cons) = creqs.addRequirement(creqs.exts, req)
      (withCReqs(creqs.copy(exts = crs)), cons)
  }

  def addRequirements(reqs: Iterable[CReq[_]]): (ClassContext, Seq[Constraint]) =
    Util.loop[ClassContext, CReq[_], Constraint](_.addRequirement(_))(this, reqs)


  def addCurrentClassRequirement(cls: Type): (ClassContext, Option[Constraint]) = creqs.currentCls match {
    case None => (withCReqs(creqs.copy(currentCls = Some(cls))), None)
    case Some(t) => (this, Some(Equal(t, cls)))
  }

  def satisfyCurrentClass(cls: CName): (ClassContext, Seq[Constraint]) = creqs.currentCls match {
    case None => (this, Seq())
    case Some(t) => (withCReqs(creqs.copy(currentCls = None)), Seq(Equal(t, cls)))
  }

  private def withCReqs(newcreqs: ClassReqs): ClassContext = this.copy(creqs = newcreqs)
}


