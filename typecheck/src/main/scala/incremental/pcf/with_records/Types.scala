package incremental.pcf.with_records

import constraints.CVar
import constraints.equality._
import constraints.equality.Type.Companion.TSubst
import incremental.pcf.UVar

/**
 * Created by seba on 15/11/14.
 */
case class TRecord(fields: Map[Symbol, Type]) extends Type {
  def occurs(x: CVar) = fields.exists(_._2.occurs(x))
  def subst(s: TSubst) = TRecord(fields.mapValues(_.subst(s)))
  def unify[CS <: ConstraintSystem[CS]](other: Type, cs: CS) = other match {
    case TRecord(fields2) if fields.keys == fields2.keys => {
      fields.keys.foldLeft(cs)((cs,k) => fields(k).unify(fields2(k), cs))
    }
    case UVar(_) => other.unify(this, cs)
    case _ => cs.never(EqConstraint(this, other))
  }
}