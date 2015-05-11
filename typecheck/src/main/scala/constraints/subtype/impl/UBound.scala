package constraints.subtype.impl

import constraints.subtype.Type
import constraints.subtype.CSubst.CSubst

case class UBound(nonground: Set[Type], ground: Option[Type]) {
  val isEmpty = nonground.isEmpty && !ground.isDefined
  val isGround = nonground.isEmpty && ground.isDefined

  def merge(that: UBound): (UBound, Set[Type]) = {
    (ground, that.ground) match {
      case (Some(t1), Some(t2)) =>
        t1 && t2 match {
          case None => (UBound(nonground ++ that.nonground, ground), Set(t1, t2))
          case meet => (UBound(nonground ++ that.nonground, meet), Set())
        }
      case _ => (UBound(nonground ++ that.nonground, ground.orElse(that.ground)), Set())
    }
  }

  //2nd component: set of types which have undefined greatest lower bound
  def add(t: Type): (UBound, Set[Type]) =
    if (t.isGround) {
      val lub = if (ground.isDefined) t && ground.get else Some(t)
      val error = if (lub.isDefined) Set[Type]() else Set(t, ground.get)
      (UBound(nonground, lub), error)
    }
    else (UBound(nonground + t, ground), Set())

  def subst(sigma: CSubst): (UBound, Set[Type]) = {
    val (g, ng) = nonground.map(_.subst(sigma)).partition(_.isGround)
    if (g.isEmpty)
      (UBound(ng, ground), Set())
    else {
      val meet: Option[Type] = g.map(Some(_)).reduce( (a: Option[Type], b: Option[Type]) => (a,b) match {
        case (Some(t1), Some(t2)) => (t1 && t2)
        case _ => None
      })

      if(meet.isDefined) {
        (meet, ground) match {
          case (Some(t1), Some(t2)) =>
            t1 && t2 match {
              case None => (UBound(ng, ground), g + ground.get)
              case meet => (UBound(ng, meet), Set())
            }
          case _ => (UBound(ng, ground.orElse(meet)), Set())
        }
      }
      else (UBound(ng, ground), g)
    }
  }
}