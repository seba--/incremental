package incremental

import constraints.Constraint

/**
 * Created by qwert on 19.10.15.
 */

class Context[C] {
  private var cs: Seq[C] = Seq()

  def addConstraint(constraint: C) = cs = constraint +: cs
  def addConstraints(cons: C*) = cs = cons ++ cs // TODO: ???
  def addConstraints(cons: Seq[C]) = cs = cons ++ cs

  def getConstraints = cs
}
