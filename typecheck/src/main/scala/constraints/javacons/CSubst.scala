package constraints.javacons

import constraints.CVar

/**
 * Created by qwert on 22.07.15.
 */

trait CTerm[CT <: CTerm[CT]] extends constraints.CTerm[Gen, Constraint, CT]

object CSubst {
  type CSubst = constraints.CSubst.CSubst[Constraint]
  def empty: CSubst = constraints.CSubst.empty[Constraint]
  def apply[CT <: CTerm[CT]](kv: (CVar[CT], CT)) = empty + kv
}