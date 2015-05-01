package constraints.subtype

import Type.Companion._

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

  def addUpperBound(v: Symbol, t: Type): CS
  def addLowerBound(v: Symbol, t: Type): CS
}