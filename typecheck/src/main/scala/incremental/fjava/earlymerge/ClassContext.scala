package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava.{Constraint, Equal, Type}
import incremental.Util
import incremental.fjava.CName


trait ClassFact
case class CtorFact(cls: CName, args: Seq[CName]) extends ClassFact
case class FieldFact(cls: CName, field: Symbol, typ: CName) extends ClassFact
case class MethodFact(cls: CName, name: Symbol, params: Seq[CName], ret: CName) extends ClassFact
case class ExtendsFact(cls: CName, ext: CName) extends ClassFact

/*
 * Invariant: All cfacts have been used to satisfy requirements in creqs.
 */
case class ClassContext(creqs: ClassReqs = ClassReqs(), cfacts: Seq[ClassFact] = Seq()) {

  def subst(s: CSubst): ClassContext = withCReqs(creqs.subst(s))

  def merge(other: ClassContext): (ClassContext, Seq[Constraint]) = {
    val (satisfiedThis, cons1) = this.addFacts(other.cfacts)
    val (satisfiedOther, cons2) = other.addFacts(this.cfacts)
    val (mergedReqs, cons3) = satisfiedThis.creqs.merge(satisfiedOther.creqs)
    (satisfiedThis.withCReqs(mergedReqs), cons1 ++ cons2 ++ cons3)
  }

  def addFact(fact: ClassFact*): (ClassContext, Seq[Constraint]) = ???

  def addFacts(facts: Iterable[ClassFact]): (ClassContext, Seq[Constraint]) =
    Util.loop[ClassContext, ClassFact, Constraint](_.addFact(_))(this, facts)

  def addRequirement(req: CReq[_]): (ClassContext, Seq[Constraint]) = req match {
    case req: CtorCReq =>
      val (crs, cons) = creqs.addRequirement(creqs.ctors, req)
      (withCReqs(creqs.copy(ctors = crs)), cons)
    case req: FieldCReq =>
      val (crs, cons) = creqs.addRequirement(creqs.fields, req)
      (withCReqs(creqs.copy(fields = crs)), cons)
    case req: MethodCReq =>
      val (crs, cons) = creqs.addRequirement(creqs.methods, req)
      (withCReqs(creqs.copy(methods = crs)), cons)
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


