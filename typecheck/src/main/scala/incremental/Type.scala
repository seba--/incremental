package incremental

/**
 * Created by seba on 13/11/14.
 */
import Type._
trait Type {
  def occurs(x: Symbol): Boolean

  def subst(s: TSubst): Type

  def unify(other: Type, s: TSubst): Option[TSubst]
  def unify(other: Type): Option[TSubst] = unify(other, Map())
}


object Type {
  type TError = String
  type TSubst = Map[Symbol, Type]
}