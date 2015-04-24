package constraints.subtype

import constraints.Typ

//Type class for types with groundness test
trait SType[T] extends Typ[T] {
  def occurs(x: Symbol): Boolean
  def subst(s: Map[Symbol, T]): T
  val isGround: Boolean
}
