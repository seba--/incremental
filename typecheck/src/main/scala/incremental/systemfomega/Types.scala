package incremental.systemfomega

import constraints.CVar
import constraints.normequality._
import constraints.normequality.CSubst.CSubst
import constraints.normequality.Type._
import incremental.Node._
import incremental.{Node_, NodeKind}

/**
 * Created by seba on 13/11/14.
 */

case class UVar(x: CVar[Type]) extends Type {
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst) = s.hgetOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (other == this) cs
    else cs.substitution.hget(x) match {
      case Some(t) => t.unify(other, cs)
      case None =>
        val t = other.subst(cs.substitution)
        if (this == t)
          cs
        else if (t.occurs(x))
          cs.never(EqConstraint(this, t))
        else
          cs.solved(CSubst(x -> t))
    }
}
object UVar {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]))) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) = UVar(lits(0).asInstanceOf[CVar[Type]])
  }
}

case object TNum extends Type {
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TNum => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }

  case object Kind extends Type.Kind(simple()) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) = TNum
  }
}

case class TFun(t1: Type, t2: Type) extends Type {
  def occurs(x: CVar[_]) = t1.occurs(x) || t2.occurs(x)
  def subst(s: CSubst) = TFun(t1.subst(s), t2.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TFun(t1_, t2_) => t2.unify(t2_, t1.unify(t1_, cs))
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}
object TFun {
  case object Kind extends Type.Kind(simple(cType, cType)) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) = TFun(getType(kids(0)), getType(kids(1)))
  }
}

case class TVar(alpha : Symbol) extends Type {
  def occurs(x2: CVar[_]) = alpha == x2
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case TVar(`alpha`) => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
object TVar {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]))) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) = TVar(lits(0).asInstanceOf[Symbol])
  }
}

case class TUniv(alpha : Symbol, k: Option[Kind], t : Type) extends Type {
  def occurs(x2: CVar[_]) = alpha == x2 || t.occurs(x2)
  def subst(s: CSubst) = TUniv(alpha, k, t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case TUniv(alpha2, k2, t2) =>
      val cs1 = (k, k2) match {
        case (Some(k1_),Some(k2_)) => k1_.unify(k2_, cs)
        case _ => cs
      }
      val cs2 = cs1.solved(CSubst(CVar[Type](alpha2) -> TVar(alpha)))
      t.unify(t2, cs2) without Set(CVar(alpha2))
    case UUniv(_, _, _) => other.unify(this, cs)
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
object TUniv {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]), cType) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cType)) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) =
      if (lits.size == 1)
        TUniv(lits(0).asInstanceOf[Symbol], None, getType(kids(0)))
      else
        TUniv(lits(0).asInstanceOf[Symbol], Some(lits(1).asInstanceOf[Kind]), getType(kids(0)))
  }
}


case class UUniv(alpha: CVar[Type], k: Kind, t : Type) extends Type {
  def occurs(x2: CVar[_]) = alpha == x2 || t.occurs(x2)
  def subst(s : CSubst) = s.hget(alpha) match {
    case Some(TVar(beta)) => TUniv(beta, Some(k), t.subst(s))
    case Some(UVar(beta)) => UUniv(beta, k, t.subst(s))
    case None => UUniv(alpha, k, t.subst(s))
    case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case UUniv(alpha2, k2, t2) =>
      val cs1 = k.unify(k2, cs)
      val cs2 = cs1.solved(CSubst(alpha -> UVar(alpha2)))
      t.unify(t2, cs2)
    case TUniv(alpha2, k2, t2) =>
      val cs1 = k2.map(k2 => k.unify(k2, cs)).getOrElse(cs)
      val cs2 = cs1.solved(CSubst(alpha -> TVar(alpha2)))
      t.unify(t2, cs2)
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
object UUniv {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]), cType) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cType)) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) =
      UUniv(lits(0).asInstanceOf[CVar[Type]], lits(1).asInstanceOf[Kind], getType(kids(0)))
  }
}

//case class TTAbs(X: Symbol, k: Option[Kind], t: Type) extends Type {
//
//}
//object TTAbs {
//  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]), cType) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cType)) {
//    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) =
//      if (lits.size == 1)
//        TTAbs(lits(0).asInstanceOf[Symbol], None, getType(kids(0)))
//      else
//        TTAbs(lits(0).asInstanceOf[Symbol], Some(lits(1).asInstanceOf[Kind]), getType(kids(0)))
//  }
//}
//
//case class TTApp(t1: Type, t2: Type) extends Type {
//
//}
//object TTApp {
//  case object Kind extends Type.Kind(simple(cType, cType)) {
//    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) = TTApp(getType(kids(0)), getType(kids(1)))
//  }
//}