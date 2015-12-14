package incremental.systemfomega

import java.util.Objects

import constraints.CVar
import constraints.normequality._
import constraints.normequality.CSubst.CSubst
import constraints.normequality.Type._
import incremental.Node._
import incremental.systemfomega.OmegaCheck._
import incremental.{Context, Node_, NodeKind}

import scala.reflect.ClassTag

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
object UVar extends TypeExtractor[UVar, CVar[Type]] {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]))) {
    override def toString() = "UVar"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) = UVar(lits(0).asInstanceOf[CVar[Type]])
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = ???
  }
}

case class TNum() extends Type {
  def occurs(x: CVar[_]) = false
  def subst(s: CSubst) = this
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TNum() => cs
    case UVar(x) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
object TNum extends TypeExtractorBoolean[TNum] {
  case object Kind extends Type.Kind(simple()) {
    override def toString() = "TNum"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) = TNum()
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = TypeResult(KStar, emptyTReqs)
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
object TFun extends TypeExtractor[TFun, (Type,Type)] {
  case object Kind extends Type.Kind(simple(cType, cType)) {
    override def toString() = "TFun"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) = TFun(getType(kids(0)), getType(kids(1)))
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
      val TypeResult(k1, treqs1) = kids(0).typ
      val TypeResult(k2, treqs2) = kids(1).typ
      val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)

      context.addConstraints(EqKindConstraint(k1, KStar), EqKindConstraint(k2, KStar))
      context.addConstraintSeq(mtcons)

      TypeResult(KStar, mtreqs)
    }
  }
}

case class TVar(alpha : Symbol) extends Type {
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
object TVar extends TypeExtractor[TVar, Symbol] {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]))) {
    override def toString() = "TVar"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) = TVar(lits(0).asInstanceOf[Symbol])
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
      val X = lits(0).asInstanceOf[Symbol]
      val K = freshKUVar()
      TypeResult(K, Map(X -> K))
    }
  }
}

case class TUniv(alpha : Symbol, k: Option[Kind], t : Type) extends Type {
  def occurs(x2: CVar[_]) = t.occurs(x2)
  def subst(s: CSubst) = TUniv(alpha, k, t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case TUniv(alpha2, k2, t2) =>
      val cs1 = (k, k2) match {
        case (Some(k1_),Some(k2_)) => k1_.unify(k2_, cs)
        case _ => cs
      }
      t.unify(t2, cs1.solved(CSubst(CVar[Type](alpha) -> TVar(alpha2)))) without Set(CVar(alpha))
    case UUniv(_, _, _) => other.unify(this, cs)
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
object TUniv extends TypeExtractor[TUniv, (Symbol, Option[Kind], Type)] {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]), cType) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cType)) {
    override def toString() = "TUniv"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) =
      if (lits.size == 1)
        TUniv(lits(0).asInstanceOf[Symbol], None, getType(kids(0)))
      else
        TUniv(lits(0).asInstanceOf[Symbol], Some(lits(1).asInstanceOf[Kind]), getType(kids(0)))
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
      val X = lits(0).asInstanceOf[Symbol]
      val TypeResult(kt, treqs) = kids(0).typ
      val ktc = EqKindConstraint(kt, KStar)

      context.addConstraint(ktc)

      if (lits.size == 2) {
        val k = lits(1).asInstanceOf[Kind]

        treqs.get(X) match {
          case None => TypeResult(k, treqs)
          case Some(k2) =>
            context.addConstraint(EqKindConstraint(k, k2))
            TypeResult(k, treqs - X)
        }
      }
      else {
        treqs.get(X) match {
          case None => TypeResult(freshKUVar(), treqs)
          case Some(k2) =>
            TypeResult(k2, treqs - X)
        }
      }
    }
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
object UUniv extends TypeExtractor[UUniv, (CVar[Type], Kind, Type)] {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]), cType) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cType)) {
    override def toString() = "UUniv"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) =
      UUniv(lits(0).asInstanceOf[CVar[Type]], lits(1).asInstanceOf[Kind], getType(kids(0)))
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
      ???
    }
  }
}

case class TTAbs(alpha: Symbol, k: Option[Kind], t: Type) extends Type {
  def occurs(x2: CVar[_]) = t.occurs(x2)
  def subst(s: CSubst) = TTAbs(alpha, k, t.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
    case TTAbs(alpha2, k2, t2) =>
      val cs1 = (k, k2) match {
        case (Some(k1_),Some(k2_)) => k1_.unify(k2_, cs)
        case _ => cs
      }
      val cs2 = cs1.solved(CSubst(CVar[Type](alpha2) -> TVar(alpha)))
      t.unify(t2, cs2) without Set(CVar(alpha2))
    // case UUniv(_, _, _) => other.unify(this, cs)
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}
object TTAbs extends TypeExtractor[TTAbs, (Symbol, Option[Kind], Type)] {
  case object Kind extends Type.Kind(simple(Seq(classOf[Symbol]), cType) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cType)) {
    override def toString() = "TTAbs"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) =
      if (lits.size == 1)
        TTAbs(lits(0).asInstanceOf[Symbol], None, getType(kids(0)))
      else
        TTAbs(lits(0).asInstanceOf[Symbol], Some(lits(1).asInstanceOf[Kind]), getType(kids(0)))
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
      ???
    }
  }
}

case class TTApp(t1: Type, t2: Type) extends Type {
  def occurs(x: CVar[_]) = t1.occurs(x) || t2.occurs(x)
  def subst(s: CSubst) = TTApp(t1.subst(s), t2.subst(s))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs :CS) = other match {
      // weaker equivalence required (e.g., beta-equiv)
    case TTApp(u1, u2) => t1.unify(u1, t2.unify(u2, cs))
    // case UUniv(_, _, _) => other.unify(this, cs)
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }

  override def isReducible = t1.isInstanceOf[TTAbs]
  override def reduce = {
    val ttabs = t1.asInstanceOf[TTAbs]
    ttabs.t.subst(CSubst(CVar[Type](ttabs.alpha) -> t2))
  }
}
object TTApp extends TypeExtractor[TTApp, (Type, Type)] {
  case object Kind extends Type.Kind(simple(cType, cType)) {
    override def toString() = "TTApp"
    def getType(lits: Seq[Lit], kids: Seq[Node_[Constraint, _, Result]]) = TTApp(getType(kids(0)), getType(kids(1)))
    def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
      ???
    }
  }
}