package incremental

/**
 * Created by seba on 13/11/14.
 */
import Type._
trait Type {
  def subst(s: TSubst): Type
  def unify(other: Type, s: TSubst): Option[TSubst]
}


object Type {
  type TError = String
  type TSubst = Map[Symbol, Type]
}