package incremental

/**
 * Created by seba on 13/11/14.
 */
import Type._
import incremental.ConstraintOps.Solution

trait Type {
  val isGround: Boolean

  def occurs(x: Symbol): Boolean

  def subst(s: TSubst): Type

  def unify(other: Type, s: TSubst): Solution
  def unify(other: Type): Solution = unify(other, Map())
}


object Type {
  type TError = String
  type TSubst = Map[Symbol, Type]
}