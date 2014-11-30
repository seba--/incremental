package incremental.pcf.with_records

import incremental.Type
import incremental.Type.TSubst
import incremental.pcf.TVar

/**
 * Created by seba on 15/11/14.
 */
case class TRecord(fields: Map[Symbol, Type]) extends Type {
  def occurs(x: Symbol) = fields.exists(_._2.occurs(x))
  def subst(s: TSubst) = TRecord(fields.mapValues(_.subst(s)))
  def unify(other: Type, s: TSubst): Option[TSubst] = other match {
    case TRecord(fields2) if fields.keys == fields2.keys => {
      var subst: TSubst = Map()
      for (k <- fields.keys)
        fields(k).unify(fields2(k), s) match {
          case None => return None
          case Some(s2) => subst = subst.mapValues(_.subst(s2)) ++ s2
        }
      Some(subst)
    }
    case TVar(_) => other.unify(this, s)
    case _ => None
  }
}