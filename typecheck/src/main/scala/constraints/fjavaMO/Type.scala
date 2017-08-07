package constraints.fjavaMO

import constraints.{CTermBase, CVar}
import constraints.fjava.CSubst.CSubst
import incremental.fjava.UCName

//Type class for types with groundness test

trait Type extends CTerm[Type] {

  def occurs(x: CVar[_]): Boolean
  def subst(s: CSubst): Type
  def isGround: Boolean
  def uvars: Set[CVar[Type]]

  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def compatibleWith(t2: Type) = Equal(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = Equal(this, t2.asInstanceOf[Type])
}

trait GroundType extends Type {
  final override def isGround = true
  final override def subst(s: CSubst) = this
}



