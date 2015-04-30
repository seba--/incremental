package incremental.pcf.with_records

import constraints.equality._
import constraints.equality.Type.Companion.TSubst

/**
 * Created by seba on 15/11/14.
 */
case class TRecord(fields: Map[Symbol, Type]) extends Type {
  def freeTVars = fields.values.foldLeft(Set[Symbol]())(_++_.freeTVars)
  def occurs(x: Symbol) = fields.exists(_._2.occurs(x))
  def normalize = TRecord(fields.mapValues(_.normalize))
  def subst(s: TSubst) = TRecord(fields.mapValues(_.subst(s)))
  def unify[CS <: ConstraintSystem[CS]](other: Type, s: TSubst)(implicit csf: ConstraintSystemFactory[CS]) = other match {
    case TRecord(fields2) if fields.keys == fields2.keys => {
      var sol = csf.emptySolution
      for (k <- fields.keys)
        sol = sol mergeSubsystem fields(k).unify(fields2(k), s)
      sol
    }
    case UVar(_) => other.unify(this, s)
    case _ => csf.never(EqConstraint(this, other))
  }
}