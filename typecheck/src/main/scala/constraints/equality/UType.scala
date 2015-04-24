package constraints.equality

import constraints.Typ
import incremental.ConstraintOps.Solution

//Type class for types which support unification
trait UType[T] extends Typ[T] {
  def occurs(x: Symbol): Boolean
  def subst(s: Map[Symbol, T]): T
  def freeTVars: Set[Symbol]
  def normalize: T
  def unify(other: T, s: Map[Symbol, T]): Solution
  def unify(other: T): Solution = unify(other, Map())
}
