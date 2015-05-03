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

case class UVar(x: Symbol) extends Type {
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def normalize = this
  def subst(s: TSubst) = s.getOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (other == this) cs
    else cs.substitution.get(x) match {
      case Some(t) => t.unify(other, cs)
      case None =>
        val t = other.subst(cs.substitution)
        if (this == t)
          cs
        else if (t.occurs(x))
          cs.never(EqConstraint(this, t))
        else
          cs.solved(Map(x -> t))
    }
}