package constraints.fjavaGen

import constraints.fjavaGen.CSubst.CSubst


abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[Gen, Constraint, CS] {

  def substitution: CSubst
  def notyet: Seq[Constraint]
  def never: Seq[Constraint]

  def addUpperBound(t1: Type, t2: Type): CS
  def extendz(t1: GroundType, t2: GroundType): CS
  def tvarBoundAdd(t1 : Seq[Type], t2: Seq[Type]): CS
  def solved(s: CSubst): CS
 // def solvedFJ( s: CSubst, ext : ExtendD) : CS
  def notyet(c: Constraint): CS
  def never(c: Constraint): CS

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: ConstraintSystem[_]
}
