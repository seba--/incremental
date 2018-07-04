package constraints.equality_letpoly

import constraints.equality_letpoly.CSubst.CSubst
import constraints.CVar

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[Gen, Constraint, CS] {

  def substitution: CSubst
  def notyet: Seq[Constraint]
  def never: Seq[Constraint]

  def solved(s: CSubst): CS
  def notyet(c: Constraint): CS
  def never(c: Constraint): CS
  def without(xs: Set[CVar[_]]): CS
  def addcompatibleCons(t1 : Type, t2 : Type): CS

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: ConstraintSystem[_]
}
