package incremental.pcf.with_records

import incremental.ConstraintOps._
import incremental.Type.Companion.TSubst
import incremental._
import incremental.pcf.UVar

/**
 * Created by seba on 13/11/14.
 */
case class EqRecordProjectConstraint(record: Type, label: Symbol, field: Type) extends Constraint {
  def solve(s: CSet) = {
    val trec = record.subst(s.substitution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(s)
        }
      case UVar(_) => notyet(EqRecordProjectConstraint(trec, label, field))
      case _ => never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def finalize(s: CSet) = {
    val trec = record.subst(s.substitution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(s)
        }
      case UVar(x) =>
        var cons = Seq[Constraint]()
        var fields = Map(label -> field.subst(s.substitution))
        for (EqRecordProjectConstraint(UVar(`x`), l, field) <- s.notyet)
          if (!fields.isDefinedAt(l))
            fields += l -> field.subst(s.substitution)
          else
            cons = EqConstraint(fields(l), field) +: cons
        solution(Map(x -> TRecord(fields))) ++ (s ++ cons)
      case _ => never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def subst(s: TSubst) = EqRecordProjectConstraint(record.subst(s), label, field.subst(s))
}
