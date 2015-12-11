package incremental.pcf.with_subtyping

import incremental.pcf.with_subtyping.SubtypingCheck._
import incremental.{NodeKind, SyntaxChecking, Context}
import incremental.Node._
import constraints.subtype._

/**
 * Created by seba on 13/11/14.
 */

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[Constraint, Result](syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Num extends Exp(simple(Seq(classOf[Integer]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = (TInteger, emptyReqs)
}
case object Add extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

    context.addConstraints(Subtype(t1, TNumeric), Subtype(t2, TNumeric))
    context.addConstraintSeq(mcons)

    (TNumeric, mreqs)
  }
}
case object Mul extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

    context.addConstraints(Subtype(t1, TNumeric), Subtype(t2, TNumeric))
    context.addConstraintSeq(mcons)

    (TNumeric, mreqs)
  }
}
case object Var extends Exp(simple(Seq(classOf[Symbol]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val x = lits(0).asInstanceOf[Symbol]
    val X = gen.freshUVar(true)
    (X, Map(x -> X))
  }
}
case object Abs extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[Type]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    if (lits(0).isInstanceOf[Symbol]) {
      val x = lits(0).asInstanceOf[Symbol]
      val annotatedT = if (lits.size == 2) lits(1).asInstanceOf[Type] else gen.freshUVar(true)
      val (t, reqs) = kids(0).typ

      reqs.get(x) match {
        case None =>
          (TFun(annotatedT, t), reqs - x)
        case Some(treq) =>
          val otherReqs = reqs - x
          context.addConstraint(Subtype(annotatedT, treq))
          (TFun(annotatedT, t), otherReqs)
      }
    } else {
      ???
    }
  }
}
case object App extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ
    val X = gen.freshUVar(false)
    val Y = gen.freshUVar(true)
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

    context.addConstraints(Equal(t1, TFun(X, Y)), Subtype(t2, X))
    context.addConstraintSeq(mcons)

    (Y, mreqs)
  }
}
case object If0 extends Exp(simple(cExp, cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ
    val (t3, reqs3) = kids(2).typ
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)
    val Xjoin = gen.freshUVar(true)

    context.addConstraints(Subtype(t1, TNumeric), Join(Xjoin, Set(t2, t3)))
    context.addConstraintSeq(mcons)

    (Xjoin, mreqs)
  }
}
case object Fix extends Exp(simple(cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t, reqs) = kids(0).typ
    val X = gen.freshUVar(false)
    val Y = gen.freshUVar(true)

    context.addConstraints(Equal(t, TFun(X, Y)), Subtype(Y, X))

    (X, reqs)
  }
}