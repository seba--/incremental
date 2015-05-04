package constraints.equality

import constraints.{CVar, TypeCompanion}


//Type that support unification
trait Type extends constraints.Type {
  def occurs(x: CVar): Boolean
  def subst(s: Type.Companion.TSubst): Type
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
}
object Type {
  implicit object Companion extends TypeCompanion {
    type TError = String
    type TSubst = Map[CVar, Type]
  }
}
import constraints.equality.Type.Companion._