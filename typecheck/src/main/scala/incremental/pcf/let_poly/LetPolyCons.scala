package incremental.pcf.let_poly

import constraints.CVar
import constraints.equality.CSubst.CSubst
import constraints.equality._
import incremental.haskell.TypeChecker

// body[alpha := substitute] = result
case class GenConstraint(ts : Type, t : Type, req: Map[Symbol, Type]) extends Constraint {
  private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS): CS = ts.unify(t, cs)

  private def unsolved[CS <: ConstraintSystem[CS]](t: Type, cs: CS): Seq[UVar] = {
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
  private def reqUnsolved[CS <: ConstraintSystem[CS]]( cs: CS): Seq[UVar] = {
    var tyvar = Seq[UVar]()
    for ((x, typ) <- req){
      tyvar = tyvar ++ unsolved(typ, cs)
    }
    tyvar
  }

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS ={
    if (t.isGround) ts.unify(t, cs)
    else {
      t match {
        case USchema(x) => ts.unify(t, cs)
        case _ =>  ts.unify(TSchema(t, unsolved(t, cs).diff(reqUnsolved(cs))), cs)
      }
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    GenConstraint(ts, t, req)
  }
}

case class InstConstraint(t1 : Type, t2 : Type, freshU: Seq[UVar]) extends Constraint {
  //private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS) : CS = t1.unify(t2, cs)

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    t1 match {
      case  USchema(x) => cs.notyet(InstConstraint(USchema(x), t2, Seq()))
      case TSchema(t, l) =>
        for (i <- 0 until l.size) {
          l(i).unify(freshU(i), cs)
        }
        t2.unify(t, cs)
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    EqConstraint(t1.subst(s), t2.subst(s))
  }
}

