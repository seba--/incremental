package incremental.pcf.let_poly

import constraints.CVar
import constraints.equality.CSubst.CSubst
import constraints.equality._
import constraints.fjava.AllEqual
import incremental.haskell.TypeChecker

// body[alpha := substitute] = result
case class GenConstraint(ts : Type, t : Type, req1: Map[Symbol, Type], req2: Map[Symbol, Type]) extends Constraint {
  private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS): CS = ts.unify(t, cs)

  private def unsolved[CS <: ConstraintSystem[CS]](t: Type, cs: CS): Seq[UVar] = {
    if (t.isGround) Seq()
    else {
      t match {
        case UVar(x) =>
          cs.substitution.hget(x) match {
            case Some(t) => Seq()
            case None => Seq(UVar(x))
          }
        case TFun(t1, t2) => unsolved(t1, cs) ++ unsolved(t2, cs)
        case _ => Seq() //TODO see again the cases of TSchema & TSchemaVar
      }
    }
  }
  private def reqUnsolved[CS <: ConstraintSystem[CS]]( cs: CS): Seq[UVar] = {
    var tyvar = Seq[UVar]()
    for ((x, typ) <- req1){
      req2.get(x) match {
        case None => tyvar
        case Some(t2) => tyvar = tyvar ++ unsolved(typ, cs)
      }
    }
    tyvar
  }

  private def occurU(t : Type) : Seq[UVar] = {
    t match {
      case TNum => Seq()
      case TChar => Seq()
      case UVar(x) => Seq(UVar(x))
      case TFun(t1, t2) => occurU(t1) ++ occurU(t2)
      case USchema(x) => Seq()
      case TSchema(t, lt) => Seq()
    }
  }

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS ={
     if (t.isGround) ts.unify(t, cs)
    else {
      t match {
        case USchema(x) => ts.unify(t, cs)
        case _ =>  ts.unify(TSchema(t, occurU(t)), cs)
      }
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    GenConstraint(ts.subst(s), t.subst(s), req1, req2)
  }
}
case class CompConstraint(t1: Type, seqU : Seq[Type] ) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    if (t1.isGround){
      var cons = Seq[Constraint]()
      for (i <- 0 until seqU.size)
        cons = cons :+ EqConstraint(t1,seqU(i))
      cs.addNewConstraints(cons)
    }
    else
      cs
  }
  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    CompConstraint(t1.subst(s), seqU.map(_.subst(s)))
  }
}

case class InstConstraint(t1 : Type, t2 : Type) extends Constraint {
  //private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS) : CS = t1.unify(t2, cs)

  def instVar[CS <: ConstraintSystem[CS]](t: UVar, lts: Seq[UVar], cs: CS): Type = {
    if (lts.isEmpty) t
    else {
      if (lts.contains(t)) {
        val typ = UVar(cs.gen.freshSymbol("x$"))
        cs.addNewConstraint(CompConstraint(t, Seq(typ)))
        typ
      }
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

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    (t1.subst(cs.substitution), t2.subst(cs.substitution)) match {
      case (_, USchema(x)) => cs.notyet(InstConstraint(t1, t2))
      case (USchema(x), _) => cs.notyet(InstConstraint(t1, t2))
      case (_, TSchema(t, lts)) => t1.unify(inst(t, lts, cs), cs)
      case (TSchema(t, lts), _) => t2.unify(inst(t, lts, cs), cs)
      case (_, _) => t1.unify(t2, cs)
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    InstConstraint(t1.subst(s), t2.subst(s))
  }
}

