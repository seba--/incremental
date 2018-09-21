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
      case TBool => Set()
      case UVar(x) => Set(UVar(x))
      case ListT(typ) => typ match {
        case None => Set()
        case Some(t) => occurU(t)
      }
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

  def equalTypes[CS <: ConstraintSystem[CS]](t1 : Type, t2 : Type, lts : Set[UVar],  cs: CS) : (Type, Type, Seq[Constraint])  = {
      (t1, t2) match {
        case (ListT(t1p), ListT(t2p)) =>
          (t1p, t2p) match {
            case (None, None) => (t1, t2, Seq())
            case (None, Some(t2s)) =>
              val typ2 = inst(t2s, lts, cs)
              (t1, typ2._1, typ2._2)
            case (Some(t1s), None) =>
              val typ1 = inst(t1s, lts, cs)
              (typ1._1, t2, typ1._2)
            case (Some(t1s), Some(t2s)) => {
              if (t1s.subst(cs.substitution) == t2s.subst(cs.substitution)) {
                val typ1 = inst(t1, lts, cs)
                (ListT(Some(typ1._1)), ListT(Some(typ1._1)), typ1._2)
              }
              else {
                val typ1 = inst(t1, lts, cs)
                val typ2 = inst(t2, lts, cs)
                (typ1._1, typ2._1, typ1._2 ++ typ2._2)
              }
            }
          }
        case (t1, ListT(t2s)) =>
          t2s match {
            case None =>
              val typ1 = inst(t1, lts, cs)
              (typ1._1, t2, typ1._2)
            case Some(t2p) =>
              val typ2 = inst(t2, lts, cs)
              val (t1p, b2, cons1) = equalTypesFun(t2p, t1, typ2._1, lts, cs)
              if (b2) {
                (typ2._1, ListT(Some(typ2._1)), typ2._2)
              }
              else {
                val typ1 = inst(t1, lts, cs)
                (typ1._1, typ2._1, typ1._2 ++ typ2._2)
            }
          }
        case (ListT(t1s), t2) =>
          t1s match {
            case None =>
              val typ2 = inst(t2, lts, cs)
              (t1, typ2._1, typ2._2)
            case Some(t1p) =>
              val typ1 = inst(t1, lts, cs)
              val (t2p, b1, cons1) = equalTypesFun(t1p, t2, typ1._1, lts, cs)
              if (b1) {
                (ListT(Some(typ1._1)), t2p, typ1._2)
              }
              else {
                val typ2 = inst(t2, lts, cs)
                (typ1._1, typ2._1, typ1._2 ++ typ2._2)
              }
          }
        case (TFun(t1p, t2p), TFun(t3, t4)) =>
          val (typ1, typ2, cons) = equalTypes(t1p, t2p, lts, cs)
          if (typ1 == typ2) {
            val (t3p, b1, cons1) = equalTypesFun(t1p, t3, typ1, lts, cs)
            val (t4p, b2, cons2) = equalTypesFun(t1p, t4, typ1, lts, cs)
            if (b1 || b2)
              (TFun(typ1, typ1), TFun(t3p, t4p), cons ++ cons1 ++ cons2)
            else {
              val (t3s, t4s, cons1) = equalTypes(t3, t4, lts, cs)
              (TFun(typ1, typ2), TFun(t3s, t4s), cons ++ cons1)
            }
          }
          else {
            val (t3p, b1, cons1) = equalTypesFun(t1p, t3, typ1, lts, cs)
            val (t4p, b2, cons2) = equalTypesFun(t1p, t4, typ1, lts, cs)
            (b1, b2) match {
              case (true, true) => (TFun(typ1, typ2), TFun(t3p, t4p), cons ++ cons1 ++ cons2)
              case (true, false) =>
                val (t4s, b4, cons4) = equalTypesFun(t2p, t4, typ2, lts, cs)
                if (b4)
                  (TFun(typ1, typ2), TFun(t3p, t4s), cons ++ cons1 ++ cons2  ++ cons4)
                else {
                  val (t3s, t4s, cons5) = equalTypes(t3, t4, lts, cs)
                  (TFun(typ1, typ2), TFun(t3s, t4s), cons ++ cons1)
                }
              case (false, true) =>
                val (t3s, b3, cons4) = equalTypesFun(t2p, t3, typ2, lts, cs)
                if (b3)
                  (TFun(typ1, typ2), TFun(t3s, t4p), cons ++ cons1 ++ cons2  ++ cons4)
                else {
                  val (t3s, t4s, cons5) = equalTypes(t3, t4, lts, cs)
                  (TFun(typ1, typ2), TFun(t3s, t4s), cons ++ cons1 ++ cons2 ++ cons5)
                }
              case (false, false) =>
                val (t3s, b3, cons3) = equalTypesFun(t2p, t3, typ2, lts, cs)
                val (t4s, b4, cons4) = equalTypesFun(t2p, t4, typ2, lts, cs)
                if (b3 || b4)
                  (TFun(typ1, typ2), TFun(t3s, t4s), cons ++ cons1 ++ cons2 ++ cons3 ++ cons4)
                else {
                  val (t3s, t4s, cons5) = equalTypes(t3, t4, lts, cs)
                  (TFun(typ1, typ2), TFun(t3s, t4s), cons ++ cons1)
                }
            }
          }
        case (t1, TFun(t3, t4)) =>
          val typ1 = inst(t1, lts, cs)
          val (t3p, b1, cons1) = equalTypesFun(t1, t3, typ1._1, lts, cs)
          val (t4p, b2, cons2) = equalTypesFun(t1, t4, typ1._1, lts, cs)
          if (b1 || b2)
            (typ1._1, TFun(t3p, t4p), cons1 ++ cons2 ++ typ1._2)
          else {
            val (t3s, t4s, cons) = equalTypes(t3, t4, lts, cs)
            (typ1._1, TFun(t3s, t4s), cons ++ typ1._2)
          }

        case (TFun(t3, t4), t2) =>
          val typ2 = inst(t2, lts, cs)
          val (t3p, b1, cons1) = equalTypesFun(t2, t3, typ2._1, lts, cs)
          val (t4p, b2, cons2) = equalTypesFun(t2, t4, typ2._1, lts, cs)
          if (b1 || b2)
            (TFun(t3p, t4p), typ2._1, cons1 ++ cons2 ++ typ2._2)
          else {
            val (t3s, t4s, cons) = equalTypes(t3, t4, lts, cs)
            (TFun(t3s, t4s), typ2._1, cons ++ typ2._2)
          }
        case (_, _) =>
          if (t1.subst(cs.substitution) == t2.subst(cs.substitution)) {
            val typ1 = inst(t1, lts, cs)
            (typ1._1, typ1._1, typ1._2)
          }
          else {
            val typ1 = inst(t1, lts, cs)
            val typ2 = inst(t2, lts, cs)
            (typ1._1, typ2._1, typ1._2 ++ typ2._2)
          }
      }
  }

  def equalTypesFun[CS <: ConstraintSystem[CS]](t1 : Type, t2 : Type, typ1: Type, lts : Set[UVar],  cs: CS) : (Type, Boolean, Seq[Constraint])  = {
    t2 match {
      case (ListT(_)) =>
        if (ListT(Some(t1)).subst(cs.substitution) == t2.subst(cs.substitution))
          ( ListT(Some(typ1)), true, Seq())
        else {
          val typ2 = inst(t2, lts, cs)
          (typ2._1, false, typ2._2)
        }
      case (TFun(t3, t4)) =>
        val (t3p, b3, cons3) = equalTypesFun(t1, t3, typ1, lts, cs)
        val (t4p, b4, cons4) = equalTypesFun(t1, t4, typ1, lts, cs)
        if (b3 || b4)
          (TFun(t3p, t4p), true, cons3 ++ cons4)
        else{
//          val (t3s, t4s, conss) = equalTypes(t3, t4, lts, cs)
         // (TFun(t3s, t4s), false, conss)
          (TFun(t3p, t4p), false, cons3 ++ cons4)
        }
      case (_) =>
        if (t1.subst(cs.substitution) == t2.subst(cs.substitution)) {
          (typ1, true, Seq())
        }
        else {
          val typ2 = inst(t2, lts, cs)
          (typ2._1, false, typ2._2)
        }
    }
  }

  def inst[CS <: ConstraintSystem[CS]](t: Type, lts: Set[UVar], cs: CS): (Type, Seq[Constraint]) = {
    t match {
      case UVar(x) => instVar(UVar(x), lts, cs)
      case ListT(typ) => typ match {
        case None => (t, Seq())
        case Some(tl) => (inst(tl, lts, cs)._1, inst(tl, lts, cs)._2)
        }
      case TFun(t1, t2) =>
        val (t1p, t2p, cons) = equalTypes(t1, t2, lts, cs)
        (TFun(t1p, t2p), cons)
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

case class ListEqConstraint(listTyp: Type, elemTyp: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS =
    if (listTyp.subst(cs.substitution).isGround )//&& elemTyp.subst(cs.substitution).isGround)
      listTyp.subst(cs.substitution).getTyp.unify(elemTyp.subst(cs.substitution).getTyp, cs)
    else
      cs.notyet(ListEqConstraint(listTyp, elemTyp))
  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = solve(cs)
  def subst(s: CSubst) = ListEqConstraint(listTyp.subst(s), elemTyp.subst(s))
}


