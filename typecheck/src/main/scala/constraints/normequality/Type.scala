package constraints.normequality

import constraints.{CTermBase, CVar}
import constraints.normequality.CSubst.CSubst
import incremental.{Node_, SyntaxChecking, NodeKind}
import incremental.Node._

//Type that supports unification
trait Type extends CTerm[Type] {
  def subst(s: CSubst): Type
  def occurs(x: CVar[_]): Boolean
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def compatibleWith(t2: Type) = EqConstraint(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = EqConstraint(this, t2.asInstanceOf[Type])
}

object Type {
  abstract class Kind(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]): Type
    def getType(e: Node_[_]): Type = e.kind.asInstanceOf[Kind].getType(e.lits, e.kids.seq)
  }

  def from(e: Node_[_]) = e.kind.asInstanceOf[Kind].getType(e.lits, e.kids.seq)

  val cType = classOf[Kind]
}
