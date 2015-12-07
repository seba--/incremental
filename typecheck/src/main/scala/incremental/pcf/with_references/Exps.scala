package incremental.pcf.with_references

import incremental.Node._
import incremental.NodeKind
import incremental.pcf.Exp
import incremental.pcf.Exp._
import incremental.pcf.PCFCheck._
import constraints.equality._
import incremental.Context

/**
 * Created by seba on 15/11/14.
 */
case object Ref extends Exp(simple(cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t, reqs) = kids(0).typ
    (TRef(t), reqs)
  }
}
case object Deref extends Exp(simple(cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t, reqs) = kids(0).typ
    val X = freshUVar()
    context.addConstraint(EqConstraint(TRef(X), t))
    (X, reqs)
  }
}
case object Assign extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
    context.addConstraint(EqConstraint(t1, TRef(t2)))
    context.addConstraintSeq(mcons)
    (TUnit, mreqs)
  }
}
case object Seq extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
    context.addConstraint(EqConstraint(TUnit, t1))
    context.addConstraintSeq(mcons)
    (t2, mreqs)
  }
}
