package incremental.pcf.with_subtyping

import incremental.Type
import incremental.pcf.{TFun, TNum, TVarInternal}

import scala.language.implicitConversions

/**
 * Created by oliver on 20.11.14.
 */
object TypeOps {
  implicit def toTypeOp(tpe: Type): TypeOps = new TypeOps(tpe)
  class TypeOps(val tpe: Type) extends AnyVal {
    def -->:(that: Type): Type = TFun(that, tpe)
  }

  object -->: {
    def unapply(t: Type): Option[(Type, Type)] = t match {
      case TFun(t1, t2) => Some((t1,t2))
      case _ => None
    }
  }
}
