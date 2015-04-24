package constraints.equality

import ConstraintSystemFactory._


//Type that support unification
trait Type extends constraints.Type {
  def occurs(x: Symbol): Boolean
  def subst(s: Map[Symbol, Type]): Type
  def freeTVars: Set[Symbol]
  def normalize: Type
  def unify(other: Type, s: Map[Symbol, Type]): ConstraintSystem
  def unify(other: Type): ConstraintSystem = unify(other, Map())

  final val emptySolution = ConstraintSystem(Map(), Seq(), Seq())
  final def solved(s: Type.TSubst): ConstraintSystem = ConstraintSystem(s, Seq(), Seq())
  final def notyet(c: Constraint): ConstraintSystem = ConstraintSystem(Map(), Seq(c), Seq())
  final def never(c: Constraint): ConstraintSystem = ConstraintSystem(Map(), Seq(), Seq(c))

}
object Type {
  type TError = String
  type TSubst = Map[Symbol, Type]
}
import constraints.equality.Type._

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