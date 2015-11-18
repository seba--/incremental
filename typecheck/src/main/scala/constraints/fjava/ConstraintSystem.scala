package constraints.fjava

import constraints.fjava.CSubst.CSubst
import incremental.fjava.CSig


abstract class ConstraintSystem[CS <: ConstraintSystem[CS]]
  extends constraints.ConstraintSystem[Gen, Constraint, CS] {

  def mergeFJavaSubsystem(that: CS, CT : Map[Type, CSig]): CS

  def substitution: CSubst
  def notyet: Seq[Constraint]
  def never: Seq[Constraint]

  def addUpperBound(t1: Type, t2: Type): CS
  def extendz(t1: Type, t2: Type): CS
  def solved(s: CSubst): CS
 // def solvedFJ( s: CSubst, ext : ExtendD) : CS
  def never(c: Constraint): CS

  def isSubtype(t1 : Type, t2 : Type): Boolean

  def unsolved = notyet ++ never
  def isSolved = notyet.isEmpty && never.isEmpty
  def solvable = !never.isEmpty
  def isSolvable: Boolean = never.isEmpty

  def tryFinalize: CS
}
