package constraints.fjava

import constraints.fjava.CSubst.CSubst


abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[Gen, Constraint, CS] {

  def substitution: CSubst
  def notyet: Seq[Constraint]
  def never: Seq[Constraint]

  def addUpperBound(t1: Type, t2: Type): CS
  def addLowerBound(t1: Type, t2: Type): CS
  def extendz(t1: Type, t2: Type): CS
  def solved(s: CSubst): CS
  def never(c: Constraint): CS

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: ConstraintSystem[_]
}
