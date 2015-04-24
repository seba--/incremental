package constraints.subtype

import constraints.Type

//Type class for types with groundness test
trait SType extends Type {
  def occurs(x: Symbol): Boolean
  def subst(s: Map[Symbol, SType]): SType
  val isGround: Boolean
}
