package constraints

import util.HMap
import util.HMap._

object CSubst {
  type CSubst[C] = NHMap[CTermBase[C], CVar]
  def empty[C]: CSubst[C] = HMap.empty
  def apply[C, V <: CTermBase[C]](kv: (CVar[V], V)) = empty[C] + kv
}
import CSubst.CSubst

case class CVar[+T](x: Symbol) extends HMap.Key[T]

trait CTermBase[C] {
  def subst(s: CSubst[C]): CTermBase[C]
  def compatibleWith(ct: CTermBase[C]): C
}

trait CTerm[G <: GenBase, C, CT <: CTerm[G, C, CT]] extends CTermBase[C] {
  def compatibleWith(ct: CT): C
  def subst(s: CSubst[C]): CT
}
