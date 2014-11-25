package incremental.pcf

import incremental.Type
import incremental.Type.TSubst

/**
 * Created by seba on 13/11/14.
 */
//trait CheckResult {
//  (Type, Map[Symbol, Type], Unsolvable)
//}

case object TNum extends Type {
  def subst(s: TSubst) = this
  def unify(other: Type, s: TSubst) = other match {
    case TNum => Some(Map())
    case TVar(x) => other.unify(this, s)
    case _ => None
  }
}

case class TVar(x: Symbol) extends Type {
  def subst(s: Map[Symbol, Type]) = s.getOrElse(x, this)
  def unify(other: Type, s: TSubst): Option[TSubst] =
    if (other == this) scala.Some(Map())
    else s.get(x) match {
      case Some(t) => t.unify(other, s)
      case None => Some(Map(x -> other.subst(s)))
    }
}

case class TFun(t1: Type, t2: Type) extends Type {
  def subst(s: Map[Symbol, Type]) = TFun(t1.subst(s), t2.subst(s))
  def unify(other: Type, s: TSubst): Option[TSubst] = other match {
    case TFun(t1_, t2_) =>
      t1.unify(t1_, s) match {
        case None => None
        case Some(s1) => t2.unify(t2_, s1) match {
          case None => None
          case Some(s2) => Some(s1.mapValues(_.subst(s2)) ++ s2)
        }
      }
    case TVar(x) => other.unify(this, s)
    case _ => None
  }
  override def toString= s"($t1 --> $t2)"
}
