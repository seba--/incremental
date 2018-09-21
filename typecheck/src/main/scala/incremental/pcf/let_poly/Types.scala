package incremental.pcf.let_poly

import constraints.equality_letpoly.CSubst
import constraints.equality_letpoly.CSubst.CSubst
import constraints.CVar
import constraints.equality_letpoly.{ConstraintSystem, ConstraintSystemFactory, EqConstraint, Type}

/**
 * Created by seba on 13/11/14.
 */

case class UVar(x: CVar[Type]) extends Type {
  def isGround = false
  def getTyp = this
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst):Type = s.hgetOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) =
    if (other == this) cs
    else if (other.isInstanceOf[USchema]) other.unify(this, cs)
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

case object TNum extends Type {
  def isGround = true
  def getTyp = this
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TNum => cs
    case UVar(x) => other.unify(this, cs)
    case USchema(x) => other.unify(this, cs)
    case TSchema(typ, lst) => typ.unify(this, cs)
    case ListT(None) => cs
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TFloat extends Type {
  def isGround = true
  def getTyp = this
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TFloat => cs
    case UVar(x) => other.unify(this, cs)
    case USchema(x) => other.unify(this, cs)
    case TSchema(typ, lts) => typ.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}


case object TChar extends Type {
  def isGround = true
  def getTyp = this
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TChar => cs
    case UVar(x) => other.unify(this, cs)
    case USchema(x) => other.unify(this, cs)
    case TSchema(typ, lst) => typ.unify(this, cs)
    case ListT(None) => cs
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TBool extends Type {
  def isGround = true
  def getTyp = this
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TBool => cs
    case UVar(x) => other.unify(this, cs)
    case USchema(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}


case class TFun(t1: Type, t2: Type) extends Type {
  def isGround = t1.isGround && t2.isGround
  def getTyp = TFun(t1.getTyp, t2.getTyp)
  def occurs(x: CVar[_]) = t1.occurs(x) || t2.occurs(x)
  def subst(s: CSubst) = {
    var args = List(t1.subst(s))
    var res = t2
    while (res.isInstanceOf[TFun]) {
      val resfun = res.asInstanceOf[TFun]
      args = resfun.t1.subst(s) :: args
      res = resfun.t2
    }
    res = res.subst(s)
    for (a <- args)
      res = TFun(a, res)
    res
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TFun(t1_, t2_) => t2.unify(t2_, t1.unify(t1_, cs))
    case USchema(x) => other.unify(this, cs)
   // case TSchema(typ, lst) => typ.unify(this, cs)
   // case InstS(typ) => typ.unify(this, cs)
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
  //
  override def toString= "TFun" //s"($t1 --> $t2)"
}

case class TSchema(typ : Type, lTyVar: Set[UVar]) extends Type {
  def isGround = true
  def getTyp = typ.getTyp //TODO how to get the Type for TSchma and relate it to ListT
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  private def occurU(t: Type): Set[UVar] = {
    t match {
      case TNum => Set()
      case TChar => Set()
      case TBool => Set()
      case UVar(x) => Set(UVar(x))
      case ListT(typ) => typ match {
        case None => Set()
        case Some(t) => occurU(t)
      }
      case TupleL(t1, t2) => occurU(t1) ++ occurU(t2)
      case TFun(t1, t2) => occurU(t1) ++ occurU(t2)
      case USchema(x) => Set()
      case TSchema(t, lt) => Set()
    }
  }
  def subst(s: CSubst) =
    if (typ.subst(s).isGround) typ.subst(s)
    else TSchema(typ.subst(s), occurU(typ.subst(s)) )
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TSchema(t2, ltvar2) => cs
    case USchema(x) => other.unify(this, cs)
    //case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class USchema(x : CVar[Type]) extends Type{
  def isGround = false
  def getTyp = this
  def freeTVars = Set()
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


case class ListT(typ: Option[Type]) extends Type {
  def isGround = typ match {
    case None => true
    case Some(t) => t.isGround
  }
  def getTyp : Type = typ match {
    case None => this
    case Some(t) => t
  }
  def freeTVars = Set()
  def occurs(x: CVar[_]) = typ match {
    case None => false
    case Some(t) => t ==x
  }

  def subst(s: CSubst) = typ match {
    case None => this
    case Some(t) => ListT(Some(t.subst(s)))
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS =
    if (this == ListT(None))
      cs
    else {
      other match {
        case ListT(typ2) => (typ, typ2) match {
          case (Some(t1), Some(t2)) => t1.unify(t2, cs)
          case (_, _) => cs
        }
        case UVar(_) => other.unify(this, cs)
        case USchema(_) => other.unify(this, cs)
        case _ => cs.never(EqConstraint(this, other))
      }
    }
}

case class TupleL(t1 : Type, t2: Type) extends Type {
  def isGround = t1.isGround && t2.isGround
  def getTyp = this
  def occurs(x: CVar[_]) = t1 == x || t2 == x
  def subst(s: CSubst) = TupleL(t1.subst(s), t2.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TupleL(t1p, t2p) =>
      t1.unify(t1p, cs)
      t2.unify(t2p, cs)
    case UVar(x) => other.unify(this, cs)
    case USchema(x) => other.unify(this, cs)
    case TSchema(typ, lts) => typ.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
