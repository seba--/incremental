package incremental.pcf.with_records

import constraints.equality.Type.Companion.TSubst
import constraints.equality._
import incremental.pcf.UVar

/**
 * Created by seba on 13/11/14.
 */
case class EqRecordProjectConstraint(record: Type, label: Symbol, field: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    val trec = record.subst(cs.substitution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => cs.never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(cs)
        }
      case UVar(_) => cs.notyet(EqRecordProjectConstraint(trec, label, field))
      case _ => cs.never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    val trec = record.subst(cs.substitution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => cs.never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(cs)
        }
      case v@UVar(x) =>
        var cons = Seq[Constraint]()
        var fields = Map(label -> field.subst(cs.substitution))
        for (EqRecordProjectConstraint(t, l, field) <- cs.notyet) t match {
          case UVar(y) if x == y || v == cs.substitution.getOrElse(y, t) =>
            if (!fields.isDefinedAt(l))
              fields += l -> field.subst(cs.substitution)
            else
              cons = EqConstraint(fields(l), field) +: cons

          case _ =>
              return cs.notyet(EqRecordProjectConstraint(trec, label, fields(label)))

        }
        cs.solved(Map(x -> TRecord(fields))) addNewConstraints cons
      case _ => cs.never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def subst(s: TSubst) = EqRecordProjectConstraint(record.subst(s), label, field.subst(s))
}
