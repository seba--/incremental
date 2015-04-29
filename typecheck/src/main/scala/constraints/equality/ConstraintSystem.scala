package constraints.equality

import Type.Companion._
import incremental.Util

abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[CS, Constraint, Type] {

  def substitution: TSubst
  def notyet: Seq[Constraint]
  def never: Seq[Constraint]

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: ConstraintSystem[_]
}
