package incremental.systemfomega

import constraints.normequality._
import constraints.normequality.Type._
import constraints.normequality.Type.Companion.TSubst
import incremental.Node._
import incremental.{Node_, NodeKind}

/**
 * Created by seba on 13/11/14.
 */

case class UVar(x: Symbol) extends Type {
  def freeTVars = Set()
  def occurs(x2: Symbol) = x == x2
  def normalize = this
  def subst(s: TSubst) = s.getOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (other == this) cs
    else cs.substitution.get(x) match {
      case Some(t) => t.unify(other, cs)
      case None =>
        val t = other.subst(cs.substitution)
        if (this == t)
          cs
        else if (t.occurs(x))
          cs.never(EqConstraint(this, t))
        else
          cs.solved(Map(x -> t))
    }
}
object UVar {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]))) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) = UVar(lits(0).asInstanceOf[Symbol])
  }
}

case object TNum extends Type {
  def freeTVars = Set()
  def occurs(x: Symbol) = false
  def normalize = this
  def subst(s: TSubst) = this
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
  def freeTVars = t1.freeTVars ++ t2.freeTVars
  def occurs(x: Symbol) = t1.occurs(x) || t2.occurs(x)
  def normalize = TFun(t1.normalize, t2.normalize)
  def subst(s: Map[Symbol, Type]) = TFun(t1.subst(s), t2.subst(s))
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
  def freeTVars = Set(alpha)
  def normalize = this
  def occurs(x2: Symbol) = alpha == x2
  def subst(s: TSubst) = this
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
  def freeTVars = t.freeTVars - alpha
  def occurs(x2: Symbol) = alpha == x2 || t.occurs(x2)
  def normalize = TUniv(alpha, k, t.normalize)
  def subst(s: Map[Symbol, Type]) = TUniv(alpha, k, t.subst(s - alpha))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case UUniv(alpha2, k2, t2) => t.unify(t2, cs.solved(Map(alpha2 -> TVar(alpha))))
    case TUniv(alpha2, k2, t2) => t.unify(t2, cs.solved(Map(alpha2 -> TVar(alpha)))) without Set(alpha2)
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


case class UUniv(alpha: Symbol, k: Option[Kind], t : Type) extends Type {
  def freeTVars = t.freeTVars
  def normalize = UUniv(alpha, k, t.normalize)
  def occurs(x2: Symbol) = alpha == x2 || t.occurs(x2)
  def subst(s : Map[Symbol, Type]) = s.get(alpha) match {
    case Some(TVar(beta)) => TUniv(beta, k, t.subst(s))
    case Some(UVar(beta)) => UUniv(beta, k, t.subst(s))
    case None => UUniv(alpha, k, t.subst(s))
    case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case UUniv(alpha2, k2, t2) => t.unify(t2, cs.solved(Map(alpha -> UVar(alpha2))))
    case TUniv(alpha2, k2, t2) => t.unify(t2, cs.solved(Map(alpha -> TVar(alpha2))))
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
object UUniv {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]), cType) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cType)) {
    def getType(lits: Seq[Lit], kids: Seq[Node_[_]]) =
      if (lits.size == 1)
        UUniv(lits(0).asInstanceOf[Symbol], None, getType(kids(0)))
      else
        UUniv(lits(0).asInstanceOf[Symbol], Some(lits(1).asInstanceOf[Kind]), getType(kids(0)))
  }
}
