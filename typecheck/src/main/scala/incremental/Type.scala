package incremental

/**
 * Created by seba on 13/11/14.
 */
import Type._
import incremental.ConstraintOps.Solution

import scala.language.implicitConversions

//implicits trick for per-type class instance common definitions
trait TypCompanion[T <: Typ[T]] extends Serializable {
  type TError = String
  type TSubst = Map[Symbol, T]
}

object TypCompanion {
  implicit def companion[T <: Typ[T]](implicit comp: TypCompanion[T]): TypCompanion[T] = comp
}

//Type class for types
trait Typ[T] {
  def occurs(x: Symbol): Boolean
  def subst(s: Map[Symbol, T]): T
}

//Type class for types which support unification
trait UType[T] extends Typ[T] {
  def freeTVars: Set[Symbol]
  def unify(other: T, s: Map[Symbol, T]): Solution
  def unify(other: T): Solution = unify(other, Map())
}

//Type class for types with groundness test
trait SType[T] extends Typ[T] {
  val isGround: Boolean
}

//always define a type class instance together with its companion
trait Type extends UType[Type]
object Type {
  implicit object Companion extends TypCompanion[Type]
}