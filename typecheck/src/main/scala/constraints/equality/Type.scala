package constraints.equality

import constraints.TypeCompanion


//Type that support unification
trait Type[CS <: ConstraintSystem[CS]] extends constraints.Type {
  def occurs(x: Symbol): Boolean
  def subst(s: Type.Companion.TSubst[CS]): Type[CS]
  def freeTVars: Set[Symbol]
  def normalize: Type[CS]
  def unify(other: Type[CS], s: Type.Companion.TSubst[CS]): CS
  def unify(other: Type[CS]): CS = unify(other, Map())
}
object Type {
  implicit object Companion extends TypeCompanion {
    type TError = String
    type TSubst[CS <: ConstraintSystem[CS]] = Map[Symbol, Type[CS]]
  }
}
import constraints.equality.Type.Companion._

case class UVar[CS <: ConstraintSystem[CS]](x: Symbol)(implicit val csFactory: ConstraintSystemFactory[CS]) extends Type[CS] {
  import csFactory._
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def normalize = this
  def subst(s: TSubst[CS]) = s.getOrElse(x, this)
  def unify(other: Type[CS], s: TSubst[CS]) =
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