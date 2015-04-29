package incremental.pcf.with_records

import constraints.equality.Type.Companion.TSubst
import constraints.equality._

/**
 * Created by seba on 13/11/14.
 */
case class EqRecordProjectConstraint(record: Type, label: Symbol, field: Type) extends Constraint {
  def solve[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS], csf: ConstraintSystemFactory[CS]): CS = {
    val trec = record.subst(s.substitution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => csf.never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(s, csf)
        }
      case UVar(_) => csf.notyet(EqRecordProjectConstraint(trec, label, field))
      case _ => csf.never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS], csf: ConstraintSystemFactory[CS]): CS = {
    val trec = record.subst(s.substitution)
    trec match {
      case TRecord(fields) =>
        fields.get(label) match {
          case None => csf.never(EqRecordProjectConstraint(trec, label, field))
          case Some(t) => EqConstraint(t, field).solve(s, csf)
        }
      case v@UVar(x) =>
        var cons = Seq[Constraint]()
        var fields = Map(label -> field.subst(s.substitution))
        for (EqRecordProjectConstraint(`v`, l, field) <- s.notyet)
          if (!fields.isDefinedAt(l))
            fields += l -> field.subst(s.substitution)
          else
            cons = EqConstraint(fields(l), field) +: cons
        csf.solved(Map(x -> TRecord(fields))) addNewConstraints cons
      case _ => csf.never(EqRecordProjectConstraint(trec, label, field))
    }
  }

  def subst(s: TSubst) = EqRecordProjectConstraint(record.subst(s), label, field.subst(s))
}
