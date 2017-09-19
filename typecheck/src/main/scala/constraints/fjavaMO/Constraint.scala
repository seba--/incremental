package constraints.fjavaMO

import constraints.CVar
import constraints.fjavaMO.CSubst.CSubst
import incremental.fjavaMO.{CName, UCName}
import incremental.fjavaMO.latemerge.Condition

trait Constraint {
  def subst(s: CSubst): Constraint

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS

  def uvars: Set[CVar[Type]]
}

//TODO lira see this again
case class MinSelC(seqCV: Seq[Type], actual: Seq[Type]) extends Constraint {
  def subst(s: CSubst) =  MinSelC(seqCV.map(_.subst(s)) , actual) //AllEqual(expected.map(_.subst(s)), actual.map(_.subst(s)))

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.addMinSel(seqCV, actual)

  override def uvars = Set() ++ seqCV.flatMap(_.uvars)
}

case class Subtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = lower.subtype(upper, cs)

  override def subst(s: CSubst): Constraint = Subtype(lower.subst(s), upper.subst(s))

  override def uvars = lower.uvars ++ upper.uvars
}


case class NotSubtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = cs.never(this)

  override def subst(s: CSubst): Constraint = NotSubtype(lower.subst(s), upper.subst(s))

  override def uvars = lower.uvars ++ upper.uvars
}

case class Equal(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = expected.unify(actual, cs)

  override def subst(s: CSubst): Constraint = Equal(expected.subst(s), actual.subst(s))

  override def uvars = expected.uvars ++ actual.uvars
}

case class NotEqual(expected: Type, actual: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = (expected, actual) match {
    case _ if expected.isGround && actual.isGround => if (expected != actual) cs.solved(Map()) else cs.never(this)
    case (UCName(x), UCName(y)) if x == y => cs.never(this)
    case (UCName(_), _) | (_, UCName(_)) => cs.notyet(this)
  }

  override def subst(s: CSubst): Constraint = NotEqual(expected.subst(s), actual.subst(s))

  override def uvars = expected.uvars ++ actual.uvars
}

case class Never(c: Constraint) extends Constraint {
  def subst(s: CSubst) = c.subst(s)

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = cs.never(this)

  override def uvars = c.uvars
}

case class AllEqual(expected: Seq[Type], actual: Seq[Type]) extends Constraint {
  if (expected.toString().contains("BSTNode") && actual.toString().contains("RBNode"))
    println(s"WARNING $this")


  def subst(s: CSubst) = AllEqual(expected.map(_.subst(s)), actual.map(_.subst(s)))

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    if (expected.size != actual.size)
      cs.never(this)
    else {
      var newcs = cs
      for (i <- 0 until expected.size)
        newcs = expected(i).unify(actual(i), newcs)
      newcs
    }
  }

  override def uvars = Set() ++ expected.flatMap(_.uvars) ++ actual.flatMap(_.uvars)
}

case class StupidCastWarning(was: Type, castTo: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = cs.never(this)

  override def subst(s: CSubst): Constraint = StupidCastWarning(was.subst(s), castTo.subst(s))

  override def uvars = was.uvars ++ castTo.uvars
}
