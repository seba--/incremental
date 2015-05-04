package constraints.normequality

import constraints.{CVar, TypeCompanion}
import incremental.{Node_, SyntaxChecking, NodeKind}
import incremental.Node._

//Type that supports unification
trait Type extends constraints.Type {
  def occurs(x: CVar): Boolean
  def subst(s: Type.Companion.TSubst): Type
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
}
object Type {
  abstract class Kind(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]): Type
    def getType(e: Node_[_]): Type = e.kind.asInstanceOf[Kind].getType(e.lits, e.kids.seq)
  }

  def from(e: Node_[_]) = e.kind.asInstanceOf[Kind].getType(e.lits, e.kids.seq)

  val cType = classOf[Kind]

  implicit object Companion extends TypeCompanion {
    type TError = String
    type TSubst = Map[CVar, Type]
  }
}
