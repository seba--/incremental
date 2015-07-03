package incremental.pcf.concurrent

/**
 * Created by oliver on 30.06.15.
 */
object Types {

  type Var = Long

  sealed trait Type {
    val isGround: Boolean
  }
  case class UVar(x: Var) extends Type {
    val isGround = false
  }
  case object TNum extends Type {
    val isGround = true
  }
  case class TFun(t1: Type, t2: Type) extends Type {
    val isGround = t1.isGround && t2.isGround
    override def toString = "TFun(...)"
  }

  def occurs(x: Var, t: Type): Boolean = t match {
    case t_ if t_.isGround => false
    case UVar(y) => x == y
    case TFun(t1, t2) => occurs(x, t1) || occurs(x, t2)
    case _ => false
  }
}
