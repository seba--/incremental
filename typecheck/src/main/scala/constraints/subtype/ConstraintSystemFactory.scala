package constraints.subtype

import constraints.subtype.CSubst.CSubst
import constraints.{ThreadedStatistics, LocalStatistics, State, Statistics}

abstract class ConstraintSystemFactory[CS <: ConstraintSystem[CS]] extends constraints.ConstraintSystemFactory[Gen, Constraint, CS] {
  def freshState = new State(new Gen, new LocalStatistics)
  def freshThreadsafeState = new State(new Gen, new ThreadedStatistics)

  def freshConstraintSystem: CS
  def emptySolution = freshConstraintSystem
  def solved(s: CSubst): CS
  def notyet(c: Constraint): CS
  def never(c: Constraint): CS
}




