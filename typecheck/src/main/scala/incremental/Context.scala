package incremental

import constraints.Constraint

/**
 * Created by qwert on 19.10.15.
 */

trait Context[C <: Constraint[_, _]] {
  var cs: Seq[C] = Seq()

  def addConstraint(constraint: C) = constraint +: cs
  def addConstraints(cons: C*) = cons :+ cs // TODO: ???
  def addConstraints(cons: Seq[C]) = cons ++ cs
}
