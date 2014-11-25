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
      case (TVar(x), TVar(y)) if x == y => true
      case (TNum, TNum) => true
      case (TFun(s1, t1), TFun(s2, t2)) => s2 < s1 && t1 < t2
      case _ => false
    }

    /**
     * LUB
     * @param that
     * @return
     */
    def |(that: Type): Type = (tpe, that) match {
      case (Top, _) | (_, Top) => Top
      case (Bot, t) => t
      case (t, Bot) => t
      case (TNum, TNum) => TNum
      case (s1 --> t1, s2 --> t2) => (s1 & s2) --> (t1 | t2)
      case _ => Top
    }

    /**
     * GLB
     * @param that
     * @return
     */
    def &(that: Type): Type = (tpe, that) match {
      case (Top, t) => t
      case (t, Top) => t
      case (Bot, _) | (_, Bot) => Bot
      case (TNum, TNum) => TNum
      case (s1 --> t1, s2 --> t2) => (s1 | s2) --> (t1 & t2)
      case _ => Bot
    }

    def -->(that: Type): Type = TFun(tpe, that)
  }

  object --> {
    def unapply(t: Type): Option[(Type, Type)] = t match {
      case TFun(t1, t2) => Some((t1,t2))
      case _ => None
    }
  }
}
