package constraints.normequality

import constraints.{CTermBase, CVar}
import constraints.normequality.CSubst.CSubst
import incremental.systemfomega.OmegaCheck.Result
import incremental.{Node_, SyntaxChecking, NodeKind}
import incremental.Node._

import scala.reflect.ClassTag

//Type that supports unification
trait Type extends CTerm[Type] {
  def subst(s: CSubst): Type
  def occurs(x: CVar[_]): Boolean
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def isReducible: Boolean = false
  def reduce: Type = this

  def compatibleWith(t2: Type) = EqConstraint(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = EqConstraint(this, t2.asInstanceOf[Type])
}

object Type {
  abstract class Kind(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[Constraint, Result](syntaxcheck) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]): Type
    def getType(e: Node_[Constraint, _, Result]): Type = e.kind.asInstanceOf[Kind].getType(e.lits, e.kids.seq)
  }

  def from(e: Node_[Constraint, _, Result]) = e.kind.asInstanceOf[Kind].getType(e.lits, e.kids.seq)

  val cType = classOf[Kind]
}


abstract class TypeExtractor[T, Extract](implicit tag: ClassTag[T]) {
  def unapply(t: T): Option[Extract]
  def unapply(other: Type): Option[Extract] =
    if (tag.runtimeClass.isInstance(other))
      unapply(other.asInstanceOf[T])
    else if (other.isReducible)
      unapply(other.reduce)
    else
      None
}
abstract class TypeExtractorBoolean[T](implicit tag: ClassTag[T]) {
  def unapply(t: T): Boolean
  def unapply(other: Type): Boolean =
    if (tag.runtimeClass.isInstance(other))
      unapply(other.asInstanceOf[T])
    else if (other.isReducible)
      unapply(other.reduce)
    else
      false
}
