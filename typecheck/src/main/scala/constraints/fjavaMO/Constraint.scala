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

case class Subtype(lower: Type, upper: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = lower.subtype(upper, cs)

  override def subst(s: CSubst): Constraint = Subtype(lower.subst(s), upper.subst(s))

  override def uvars = lower.uvars ++ upper.uvars
}

case class AllSubtypel(expected: Seq[Type], actual: Seq[Type]) extends Constraint {
  if (expected.toString().contains("BSTNode") && actual.toString().contains("RBNode"))
    println(s"WARNING $this")

  def subst(s: CSubst) = AllEqual(expected, actual)

  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    if (expected.size != actual.size)
      cs.never(this)
    else {
      var newcs = cs
      for (i <- 0 until expected.size)
        newcs = expected(i).subtype(actual(i), newcs)
      newcs
    }
  }

  override def uvars = Set()
}

case class MinSel(cvar: Seq[Type], setT: Seq[Seq[Type]], lowerB: Seq[Type]) extends Constraint {

  def subst(s: CSubst) = MinSel(cvar.map(_.subst(s)), setT, lowerB)

  def minsel(setT: Seq[Seq[Type]], lowerB:  Seq[Type]) : Seq[Type] = {
    val len = lowerB.length
    var res =  Seq.fill(len)(CName('Object))
    for (i <- 0 until setT.size)
        if isSubtype()
      res
  }
  def solve[CS <: ConstraintSystem[CS]](cs: CS) = {
    if (cvar.size != lowerB.size)
      cs.never(this)
    else {
      var newcs = cs
      for (i <- 0 until cvar.size)
        newcs = cvar(i).unify(minsel(setT,lowerB)(i), newcs)
      newcs
    }
  }

  override def uvars = Set() ++ cvar.flatMap(_.uvars)
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
