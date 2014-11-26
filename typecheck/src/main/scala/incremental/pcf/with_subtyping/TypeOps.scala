package incremental.pcf.with_subtyping

import incremental.Type
import incremental.pcf.{TFun, TNum, TVar}

import scala.language.implicitConversions

/**
 * Created by oliver on 20.11.14.
 */
object TypeOps {
  implicit def toTypeOp(tpe: Type): TypeOps = new TypeOps(tpe)
  class TypeOps(val tpe: Type) extends AnyVal {
    /**
     * Check if tpe is a subtype of that.
     *
     * @param that
     * @return
     */
    def <(that: Type): Boolean = (tpe, that) match {
      case (_, Top) | (Bot, _) => true
      case (TNum, TNum) => true
      case (TFun(s1, t1), TFun(s2, t2)) => s2 < s1 && t1 < t2
      case _ => false
    }

    def -->:(that: Type): Type = TFun(that, tpe)
  }

  object -->: {
    def unapply(t: Type): Option[(Type, Type)] = t match {
      case TFun(t1, t2) => Some((t1,t2))
      case _ => None
    }
  }
}
