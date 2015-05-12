package constraints.equality

import constraints.equality.CSubst.CSubst
import constraints.{CTermBase, CVar}

//Type that support unification
trait Type extends CTerm[Type] {
  def occurs(x: CVar[_]): Boolean
  def subst(s: CSubst): Type
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS

  def compatibleWith(t2: Type) = EqConstraint(this, t2)
  def compatibleWith(t2: CTermBase[Constraint]) = EqConstraint(this, t2.asInstanceOf[Type])

}
