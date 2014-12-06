package incremental.pcf.with_records

import incremental.{EqConstraint, Type}
import incremental.ConstraintOps._
import incremental.Type.Companion.TSubst
import incremental.pcf.TVar

/**
 * Created by seba on 15/11/14.
 */
case class TRecord(fields: Map[Symbol, Type]) extends Type {
  def occurs(x: Symbol) = fields.exists(_._2.occurs(x))
  def subst(s: TSubst) = TRecord(fields.mapValues(_.subst(s)))
  def unify(other: Type, s: TSubst) = other match {
    case TRecord(fields2) if fields.keys == fields2.keys => {
      var sol = emptySol
      for (k <- fields.keys)
        sol = sol ++ fields(k).unify(fields2(k), s)
      sol
    }
    case TVar(_) => other.unify(this, s)
    case _ => never(EqConstraint(this, other))
  }
}