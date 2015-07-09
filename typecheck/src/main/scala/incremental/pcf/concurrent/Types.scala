package incremental.pcf.concurrent

import scala.annotation.tailrec
import scala.collection.BitSet

/**
 * Created by oliver on 30.06.15.
 */
object Types {

  type Var = Long

  sealed trait Type {
    def isGround: Boolean
  }
  case class RVar(x: Var) extends Type {
    private[concurrent] var _forward: Option[Type] = None
    def forward = _forward.get
    def isGround = _forward.fold(false)(_.isGround)
    override def toString = if (_forward.isDefined) s"$x~><${_forward.get}>" else super[Product].toString
  }

  @tailrec
  private[concurrent] def resolve(t: Type): Type = {
    if (t.isInstanceOf[RVar]) {
      val uv = t.asInstanceOf[RVar]
      if (uv._forward.isDefined)
        resolve(uv._forward.get)
      else t
    }
    else t
  }

  object UVar {
    def unapply(t: RVar): Option[Var] = {
      val tr = resolve(t)
      if (tr.isInstanceOf[RVar]) {
        Some(tr.asInstanceOf[RVar].x)
      }
      else None
    }
  }
  case object TNum extends Type {
    val isGround = false
    def unapply(t: Type): Boolean = resolve(t) == this
  }
  case class TFun(t1: Type, t2: Type) extends Type {
    def isGround = t1.isGround && t2.isGround
    override def toString = "TFun(...)"
  }
  object TFun {
    def unapply(t: Type): Option[(Type, Type)] = {
      val tr = resolve(t)
      if (tr.isInstanceOf[TFun]) {
        val tf = tr.asInstanceOf[TFun]
        Some((tf.t1, tf.t2))
      }
      else None
    }
  }

  def occurs(x: Var, t: Type): Boolean = t match {
    case t_ if t_.isGround => false
    case UVar(y) => x == y
    case TFun(t1, t2) => occurs(x, t1) || occurs(x, t2)
    case _ => false
  }
}
