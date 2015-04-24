package incremental

/**
 * Created by seba on 13/11/14.
 */
import Type._
import constraints.{Typ, TypCompanion}
import constraints.unification.UType
import incremental.ConstraintOps.Solution

import scala.language.implicitConversions


//Type class for types with groundness test
trait SType[T] extends Typ[T] {
  def occurs(x: Symbol): Boolean
  def subst(s: Map[Symbol, T]): T
  val isGround: Boolean
}

//always define a type class instance together with its companion
trait Type extends UType[Type]
object Type {
  implicit object Companion extends TypCompanion[Type]
}