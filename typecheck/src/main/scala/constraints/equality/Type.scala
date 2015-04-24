package constraints.equality

import ConstraintSystemFactory._
import constraints.TypeCompanion


//Type that support unification
trait Type extends constraints.Type {
  def occurs(x: Symbol): Boolean
  def subst(s: Type.Companion.TSubst): Type
  def freeTVars: Set[Symbol]
  def normalize: Type
  def unify(other: Type, s: Type.Companion.TSubst): ConstraintSystem
  def unify(other: Type): ConstraintSystem = unify(other, Map())
}
object Type {
  implicit object Companion extends TypeCompanion {
    type TError = String
    type TSubst = Map[Symbol, Type]
  }
}
import constraints.equality.Type.Companion._

case class UVar(x: Symbol) extends Type {
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def normalize = this
  def subst(s: TSubst) = s.getOrElse(x, this)
  def unify(other: Type, s: TSubst) =
    if (other == this) emptySolution
    else s.get(x) match {
      case Some(t) => t.unify(other, s)
      case None =>
        val t = other.subst(s)
        if (this == t)
          emptySolution
        else if (t.occurs(x))
          never(EqConstraint(this, t))
        else
          solved(Map(x -> t))
    }
}