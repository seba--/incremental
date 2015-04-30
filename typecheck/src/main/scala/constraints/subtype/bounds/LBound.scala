package constraints.subtype.bounds

import constraints.subtype.Type
import constraints.subtype.Type.Companion.TSubst

case class LBound(nonground: Set[Type], ground: Option[Type]) {
  val isEmpty = nonground.isEmpty && !ground.isDefined
  val isGround = nonground.isEmpty && ground.isDefined

  def merge(that: LBound): (LBound, Set[Type]) = {
    (ground, that.ground) match {
      case (Some(t1), Some(t2)) =>
        t1 || t2 match {
          case None => (LBound(nonground ++ that.nonground, ground), Set(t1,t2))
          case lub => (LBound(nonground ++ that.nonground, lub), Set())
        }
      case _ => (LBound(nonground ++ that.nonground, ground.orElse(that.ground)), Set())
    }
  }

  //2nd component: set of types which have undefined least upper bound
  def add(t: Type): (LBound, Set[Type]) =
    if (t.isGround) {
      val lub = if (ground.isDefined) t || ground.get else Some(t)
      val error = if (lub.isDefined) Set[Type]() else Set(t, ground.get)
      (LBound(nonground, lub), error)
    }
    else (LBound(nonground + t, ground), Set())

  def subst(sigma: TSubst): (LBound, Set[Type]) = {
    val (g, ng) = nonground.map(_.subst(sigma)).partition(_.isGround)
    if (g.isEmpty)
      (LBound(ng, ground), Set())
    else {
      val lub: Option[Type] = g.map(Some(_)).reduce( (a: Option[Type], b: Option[Type]) => (a,b) match {
        case (Some(t1), Some(t2)) => (t1 || t2)
        case _ => None
      })

      if(lub.isDefined) {
        (lub, ground) match {
          case (Some(t1), Some(t2)) =>
            t1 || t2 match {
              case None => (LBound(ng, ground), g + ground.get)
              case lub => (LBound(ng, lub), Set())
            }
          case _ => (LBound(ng, ground.orElse(lub)), Set())
        }
      }
      else (LBound(ng, ground), g)
    }
  }
}