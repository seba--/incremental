package constraints

import constraints.CSubst.CSubst

trait Constraint[G <: GenBase, C] {
  def subst(s: CSubst[C]): C
}
