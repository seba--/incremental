package constraints.equality

import Type.Companion._
import incremental.Util

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[CS, EqConstraint, Type] {

  def substitution: TSubst
  def notyet: Seq[EqConstraint]
  def never: Seq[EqConstraint]

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: ConstraintSystem[_]
}
