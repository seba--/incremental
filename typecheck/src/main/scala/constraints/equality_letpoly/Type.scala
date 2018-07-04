package constraints.equality_letpoly

import constraints.equality_letpoly.CSubst.CSubst
import constraints.{CTermBase, CVar}

//Type that support unification
trait Type extends CTerm[Type] {
  def isGround : Boolean
  def occurs(x: CVar[_]): Boolean
  def subst(s: CSubst): Type
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def compatibleWith(t2: Type) = EqConstraint(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = EqConstraint(this, t2.asInstanceOf[Type])
}

trait GroundType extends Type {
  final override def isGround = true
  final override def subst(s: CSubst) = this
}
