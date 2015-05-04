package constraints.equality

import Type.Companion._
import incremental.Util

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[CS, Constraint, Type, Gen] {

  def substitution: TSubst
  def notyet: Seq[Constraint]
  def never: Seq[Constraint]

  def solved(s: TSubst): CS
  def notyet(c: Constraint): CS
  def never(c: Constraint): CS
  def without(xs: Set[Symbol]): CS

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: ConstraintSystem[_]
}
