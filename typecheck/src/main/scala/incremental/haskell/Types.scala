package incremental.haskell

import constraints.CVar
import constraints.equality.CSubst.CSubst
import constraints.equality._
import incremental.haskell.TypeCheckerFactory
import incremental.Util



/**
 * Created by seba on 13/11/14.
 */

trait HaskellType extends constraints.equality.Type {
  def freeTVars: Set[Symbol]
 // def unifyInst[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS
  def getFreeTVars(t: Type): Set[Symbol] =
    if (t.isInstanceOf[HaskellType])
      t.asInstanceOf[HaskellType].freeTVars
    else
      Set()
}

case class ClassH(x: Symbol) extends Type {
  def isGround = true
  def freeTVars = Set()
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other match {
    case ClassH(`x`) => cs
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
case class TCon(x: Symbol) extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other match {
    case TCon(`x`) => cs
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class SeqH(elem: Type) extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = elem == x
  def subst(s: CSubst) = SeqH(elem.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other match {
    case SeqH(elems2) => cs
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class ListH(elem: Type) extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = elem == x
  def subst(s: CSubst) = ListH(elem.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other match {
    case SeqH(elems2) => cs
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class TupleH(elems: List[Type]) extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = elems.exists(_.occurs(x))
  def subst(s: CSubst) = TupleH(elems.map(_.subst(s)))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS): CS = other match {
    case TupleH(elems2) => cs
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}


//TODO I do not know how to express tuple (when the return type it should be a tuple, or an immutable list with differnt types)

case object TBool extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TBool => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TChar extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TChar => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
case object TString extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TString => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TInt extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TInt => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TFloat extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TFloat => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}


case object TDouble extends HaskellType {
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TDouble => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TRational extends HaskellType{
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TRational => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case object TNone extends HaskellType{ //when it should return nothing
  def isGround = true
  def freeTVars = Set()
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TNone => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}


case class UVar(x: CVar[Type]) extends HaskellType{
  def isGround = false
  def freeTVars = Set()
  def occurs(x2: CVar[_]) = x == x2
  def subst(s: CSubst) = s.hgetOrElse(x, this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TSchemaVar(x) =>  other.unify(this, cs)//cs.never(EqConstraint(this, other))
    case _ =>
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
}

case class TFun(t1: Type, t2: Type) extends HaskellType{
  def isGround = false
  def freeTVars = getFreeTVars(t1) ++ getFreeTVars(t2)
  def occurs(x: CVar[_]) = t1.occurs(x) || t2.occurs(x)
  def subst(s: CSubst) = TFun(t1.subst(s), t2.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TFun(t1_, t2_) => t2.unify(t2_, t1.unify(t1_, cs))
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
  override def toString= s"($t1 --> $t2)"
}

case class TVar(alpha : Symbol) extends HaskellType{
  def isGround = true
  def freeTVars = Set(alpha)
  def occurs(x2: CVar[_]) = alpha == x2
  def subst(s: CSubst) = s.hgetOrElse(CVar[Type](alpha), this)
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) =
    if (this == other) cs
    else cs.substitution.hget(CVar[Type](alpha)) match {
      case None => other match {
        case UVar(x) => other.unify(this, cs)
        case _ => cs.never(EqConstraint(this, other))
      }
      case Some(t) => t.unify(other, cs)
    }
}

case class TUniv(alpha : Symbol, t : Type) extends HaskellType{
  def isGround = false
  def freeTVars = getFreeTVars(t) - alpha
  def occurs(x2: CVar[_]) = t.occurs(x2)
  def subst(s: CSubst) = TUniv(alpha, t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case UUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(alpha2 -> TVar(alpha))))
    case TUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(CVar[Type](alpha) -> TVar(alpha2)))) without Set(CVar(alpha))
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class UUniv(alpha: CVar[Type], t : Type) extends HaskellType{
  def isGround = false
  def freeTVars = getFreeTVars(t)
  def occurs(x2: CVar[_]) = alpha == x2 || t.occurs(x2)
  def subst(s : CSubst) = s.hget(alpha) match {
    case Some(TVar(beta)) => TUniv(beta, t.subst(s))
    case Some(UVar(beta)) => UUniv(beta, t.subst(s))
    case None => UUniv(alpha, t.subst(s))
    case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
  }
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case UUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(alpha -> UVar(alpha2))))
    case TUniv(alpha2, t2) => t.unify(t2, cs.solved(CSubst(alpha -> TVar(alpha2))))
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}

case class TSchema(typ : Type, lTyVar: Seq[UVar]) extends HaskellType{
    def isGround = true
    def freeTVars = Set()
    def occurs(x: CVar[_]) = false
    def subst(s: CSubst) = typ.subst(s)
    def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
      case TSchema(t2, ltvar2) => cs
      case TSchemaVar(x) => other.unify(this, cs)
        //case UVar(x) => other.unify(this, cs)
      case _ => cs.never(EqConstraint(this, other))
    }
}

case class TSchemaVar(x : CVar[Type]) extends HaskellType{
  def isGround = false
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
          cs.solved(CSubst(TSchemaVar(x).x -> t))
    }
}

case class InstS(typ : Type) extends HaskellType {
  def isGround = false

  def freeTVars = Set()

  def occurs(x2: CVar[_]) = x2 == typ

  def instanti(typ : Type):Type = {
    if (typ.isGround) typ
    else {
      typ match {
        case UVar(x) => UVar(x)
        case TFun(t1, t2) => TFun(instanti(t1), instanti(t2))
        case _ => InstS(typ)
      }
    }
  }
  def subst(s: CSubst) = instanti(typ.subst(s))
//    if (typ.subst(s).isGround) typ.subst(s)
//    else if (typ.subst(s).isInstanceOf[UVar]) typ.subst(s)
//    else if (typ.subst(s).isInstanceOf[TFun])
//    else InstS(typ.subst(s))


  def instVar[CS <: ConstraintSystem[CS]](t: UVar, lts: Seq[UVar], cs: CS): Type = {
    if (lts.isEmpty) t
    else {
      if (lts.contains(t))
        UVar(cs.gen.freshSymbol("x$"))
      else t
    }
  }

  def inst[CS <: ConstraintSystem[CS]](t: Type, lts: Seq[UVar], cs: CS): Type = {
    t match {
      case UVar(x) => instVar(UVar(x), lts, cs)
      case TFun(t1, t2) => TFun(inst(t1, lts, cs), inst(t2, lts, cs))
      case _ => t
    }
  }
  def instFresh[CS <: ConstraintSystem[CS]](typ: Type, cs: CS): Type = {
    typ match {
      case TSchemaVar(x) => InstS(TSchemaVar(x))
      case UVar(x) => UVar(x)
      case TSchema(t, lts) => inst(t, lts, cs)
      case _ => typ
    }
  }

  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case UVar(x) => other.unify(instFresh(this, cs), cs)
    //case InstS(t2) => instFresh(other, cs).unify((instFresh(this, cs)), cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}