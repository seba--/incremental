package incremental.pcf.let_poly

import constraints.CVar
import constraints.equality_letpoly.CSubst.CSubst
import constraints.equality_letpoly._
import constraints.fjava.AllEqual
import incremental.haskell.TypeChecker

// body[alpha := substitute] = result
case class GenConstraint(ts : Type, t : Type, req1: Map[Symbol, Type], req2: Map[Symbol, Type]) extends Constraint {
  private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS): CS = ts.unify(t, cs)

  private def unsolved[CS <: ConstraintSystem[CS]](t: Type, cs: CS): Set[UVar] = {
    if (t.isGround) Set()
    else {
      t match {
        case UVar(x) =>
          cs.substitution.hget(x) match {
            case Some(t) => Set()
            case None => Set(UVar(x))
          }
        case TFun(t1, t2) => unsolved(t1, cs) ++ unsolved(t2, cs)
        case _ => Set() //TODO see again the cases of TSchema & TSchemaVar
      }
    }
  }
  private def reqUnsolved[CS <: ConstraintSystem[CS]]( cs: CS): Set[UVar] = {
    var tyvar = Set[UVar]()
    for ((x, typ) <- req1){
      req2.get(x) match {
        case None => tyvar
        case Some(t2) => tyvar = tyvar ++ unsolved(typ, cs)
      }
    }
    tyvar
  }

  private def occurU(t : Type) : Set[UVar] = {
    t match {
      case TNum => Set()
      case TChar => Set()
      case UVar(x) => Set(UVar(x))
      case TFun(t1, t2) => occurU(t1) ++ occurU(t2)
      case USchema(x) => Set()
      case TSchema(t, lt) => Set()
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
case class CompConstraint(t1: Type, t2 : Type ) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    cs.addcompatibleCons(t1, t2)
//    if (t1.isGround){
//      var cons = Seq[Constraint]()
//      for (i <- 0 until seqU.size)
//        cons = cons :+ EqConstraint(t1,seqU(i))
//      cs.addNewConstraints(cons)
//    }
//    else
//      cs.notyet(CompConstraint(t1, seqU))
  }
  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    CompConstraint(t1.subst(s), t2.subst(s))
  }
}

case class InstConstraint(t1 : Type, t2 : Type) extends Constraint {
  //private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS) : CS = t1.unify(t2, cs)

  def instVar[CS <: ConstraintSystem[CS]](t: UVar, lts: Set[UVar], cs: CS): (Type, Seq[Constraint]) = {
    if (lts.isEmpty) (t, Seq())
    else {
      if (lts.contains(t)) {
        val typ = UVar(cs.gen.freshSymbol("x$"))
        cs.addcompatibleCons(t, typ)
        (typ, Seq(CompConstraint(t, typ)))
      }
      else (t, Seq())
    }
  }

  def inst[CS <: ConstraintSystem[CS]](t: Type, lts: Set[UVar], cs: CS): (Type, Seq[Constraint]) = {
    t match {
      case UVar(x) => instVar(UVar(x), lts, cs)
      case TFun(t1, t2) =>
        if (t1.subst(cs.substitution) == t2.subst(cs.substitution) ) {
          val typ1 = inst(t1, lts, cs)
          (TFun(typ1._1, typ1._1), typ1._2)
        }
        else {
          val typ1 = inst(t1, lts, cs)
          val typ2 = inst(t2, lts, cs)
          (TFun(typ1._1, typ2._1), typ1._2 ++ typ2._2)
        }
      case _ => (t, Seq())
    }
  }

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    (t1, t2) match {
      case (_, USchema(x)) => cs.notyet(InstConstraint(t1, t2))
      case (USchema(x), _) => cs.notyet(InstConstraint(t1, t2))
      case (_, TSchema(t, lts)) =>
        val typ =  inst(t, lts, cs)
        t1.unify(typ._1,  cs.addNewConstraints(typ._2))
      case (TSchema(t, lts), _) =>
        val typ =  inst(t, lts, cs)
        t1.unify(typ._1,  cs.addNewConstraints(typ._2))
      case (_, _) => t1.unify(t2, cs)
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    InstConstraint(t1.subst(s), t2.subst(s))
  }
}

