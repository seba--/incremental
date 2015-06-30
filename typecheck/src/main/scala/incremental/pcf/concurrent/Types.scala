package incremental.pcf.concurrent

/**
 * Created by oliver on 30.06.15.
 */
object Types {

  type Var = Long

  sealed trait Type
  case class UVar(x: Var) extends Type
  case object TNum extends Type
  case class TFun(t1: Type, t2: Type) extends Type {
    override def toString = "TFun(...)"
  }

  def occurs(x: Var, t: Type): Boolean = t match {
    case UVar(y) => x == y
    case TFun(t1, t2) => occurs(x, t1) || occurs(x, t2)
    case _ => false
  }


}
