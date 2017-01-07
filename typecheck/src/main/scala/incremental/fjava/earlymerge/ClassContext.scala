package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava.{Constraint, Equal, Type}
import incremental.Util
import incremental.fjava.CName


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
    (satisfiedThisExt.withCReqs(mergedReqs), cons1 ++ cons2 ++ cons3 ++ cons4 ++ cons5)
  }

  def addFact(fact: ClassFact): (ClassContext, Seq[Constraint]) = fact match {
    case CtorFact(cls, args) =>
      val (crs, cons) = creqs.satisfyCReq(CtorCReq(cls, args), creqs.ctors)
      val cctx = ClassContext(creqs.copy(ctors = crs), fact +: cfacts, extFacts)
      (cctx, cons)

    case FieldFact(cls, field, typ) =>
      val (crs, cons) = creqs.satisfyCReqMap(FieldCReq(cls, field, typ), creqs.fields)
      val cctx = ClassContext(creqs.copy(fields = crs), fact +: cfacts, extFacts)
      (cctx, cons)

    case MethodFact(cls, name, params, ret) =>
      val (crsMethods, cons1) = creqs.satisfyCReqMap(MethodCReq(cls, name, params, ret), creqs.methods)
      val (crsOptMethods, cons2) = creqs.satisfyCReqMap(MethodCReq(cls, name, params, ret), creqs.optMethods)
      val cctx = ClassContext(creqs.copy(methods = crsMethods, optMethods = crsOptMethods), fact +: cfacts, extFacts)
      (cctx, cons1 ++ cons2)
  }

  def getTopExtends(cls: CName): CName = {
    var top = cls
    while (true)
      extFacts.get(top) match {
        case None => return top
        case Some(newtop) =>
          top = newtop
      }
    ???
  }

  def addExtFact(cls: CName, sup: CName) = {
    var top = getTopExtends(sup)

    val (crs, cons) = creqs.satisfyCReq(ExtCReq(cls, top), creqs.exts)
    val newFields = creqs.fields.mapValues(set => set.flatMap(_.satisfyExt(cls, top))).view.force
//    println(newFields)
    val newMethods = creqs.methods.mapValues(set => set.flatMap(_.satisfyExt(cls, top))).view.force
//    println(newMethods)
    val newOptMethods = creqs.optMethods.mapValues(set => set.flatMap(_.satisfyExt(cls, top))).view.force
//    println(newOptMethods)
    val cctx = ClassContext(creqs.copy(exts = crs, fields = newFields, methods = newMethods, optMethods = newOptMethods), cfacts, extFacts + (cls -> sup))
    (cctx, cons)
  }

  def addFacts(facts: Iterable[ClassFact]): (ClassContext, Seq[Constraint]) =
    Util.loop[ClassContext, ClassFact, Constraint](_.addFact(_))(this, facts)

  def addExtFacts(facts: Iterable[(CName, CName)]): (ClassContext, Seq[Constraint]) =
    Util.loop[ClassContext, (CName, CName), Constraint]((o, in) => o.addExtFact(in._1, in._2))(this, facts)

  def addRequirement(req: CReq[_]): (ClassContext, Seq[Constraint]) = req match {
    case req: CtorCReq =>
      val (crs, cons) = creqs.addRequirement(creqs.ctors, req)
      (withCReqs(creqs.copy(ctors = crs)), cons)

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


