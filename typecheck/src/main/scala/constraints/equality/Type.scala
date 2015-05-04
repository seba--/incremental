package constraints.equality

import constraints.TypeCompanion


//Type that support unification
trait Type extends constraints.Type {
  def occurs(x: Symbol): Boolean
  def subst(s: Type.Companion.TSubst): Type
  def freeTVars: Set[Symbol]
  def normalize: Type
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
}
object Type {
  implicit object Companion extends TypeCompanion {
    type TError = String
    type TSubst = Map[Symbol, Type]
  }
}
import constraints.equality.Type.Companion._