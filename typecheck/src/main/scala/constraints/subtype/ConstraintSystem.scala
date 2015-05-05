package constraints.subtype

import constraints.CVar
import constraints.subtype.CSubst.CSubst

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[Gen, Constraint, CS] {

  def substitution: CSubst
  def notyet: Seq[Constraint]
  def never: Seq[Constraint]

  def addUpperBound(v: CVar[Type], t: Type): CS
  def addLowerBound(v: CVar[Type], t: Type): CS
  def never(c: Constraint): CS

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: ConstraintSystem[_]
}
