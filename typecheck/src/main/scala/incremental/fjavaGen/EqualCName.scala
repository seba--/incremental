package incremental.fjavaGen

import constraints.CVar
import constraints.fjavaGen.CSubst.CSubst
import constraints.fjavaGen._
import incremental.fjavaGen.latemerge.FieldCReq

// body[alpha := substitute] = result
case class EqualCName(creq1 : FieldCReq, creq2: FieldCReq) extends Constraint {
  private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS) : CS = t.unify(getApplyPos(creq1.cls.subst(cs.substitution).asInstanceOf[CName], creq2.cls.subst(cs.substitution).asInstanceOf[CName], creq1.typ), cs)

  def getApplyPos(cls : CName, instClas: CName,  f : Type ) : Type = {
    var pos : Int = 0
    for (i <- 0 until cls.params.length) {
      if (cls.params(i) == f)
        pos = i
      else pos
    }
    instClas.params(pos)
  }

  def uvars: Set[CVar[Type]] = ???

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
   if (creq2.cls.isInstanceOf[CName])
     cs.addNewConstraint(Equal(getApplyPos(creq1.cls.asInstanceOf[CName], creq2.cls.asInstanceOf[CName], creq1.typ), creq2.typ))
   else cs.notyet(EqualCName(creq1, creq2))
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    (creq1.subst(s), creq2.subst(s)) match {
      case (Some(f1), Some(f2)) => EqualCName(f1, f2)
      case (_, _) => this
    }
  }
}