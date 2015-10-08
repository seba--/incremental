package constraints.fjava

import constraints.{CTermBase, CVar}
import constraints.fjava.CSubst.CSubst

//Type class for types with groundness test
trait Type extends CTerm[Type] {

  def occurs(x: CVar[_]): Boolean
  def subst(s: CSubst): Type
  val isGround: Boolean

  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def subtype[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def extendz[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def compatibleWith(t2: Type) = Equal(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = Equal(this, t2.asInstanceOf[Type])
}





