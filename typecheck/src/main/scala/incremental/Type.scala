package incremental

/**
 * Created by seba on 13/11/14.
 */
import Type._
import constraints.{Typ, TypCompanion}
import constraints.equality.UType
import incremental.ConstraintOps.Solution

import scala.language.implicitConversions




//always define a type class instance together with its companion
trait Type extends UType[Type]
object Type {
  implicit object Companion extends TypCompanion[Type]
}