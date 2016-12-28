package incremental.fjava.earlymerge

import constraints.fjava.CSubst.CSubst
import constraints.fjava.{Constraint, Equal, Type}
import incremental.Util
import incremental.fjava.CName



case class ClassFacts() {

}

trait ClassFact
case class CtorFact(cls: CName, args: Seq[CName]) extends ClassFact
case class FieldFact(cls: CName, field: Symbol, typ: CName) extends ClassFact
case class MethodFact(cls: CName, name: Symbol, params: Seq[CName], ret: CName) extends ClassFact
case class ExtendsFact(cls: CName, ext: CName) extends ClassFact

/*
 * Invariant: All cfacts have been used to satisfy requirements in creqs.
 */
case class ClassContext(creqs: ClassReqs = ClassReqs(), cfacts: ClassFacts = ClassFacts()) {
  def subst(s: CSubst): ClassContext = ???
  def merge(other: ClassContext): (ClassContext, Seq[Constraint]) = ???

  def addFact(fact: ClassFact*): (ClassContext, Seq[Constraint]) = ???

  def addFacts(facts: Iterable[ClassFact]): (ClassContext, Seq[Constraint]) =
    Util.loop[ClassContext, ClassFact, Constraint](_.addFact(_))(this, facts)

  def addRequirement(creq: CReq[_]*): (ClassContext, Seq[Constraint]) = ???

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


