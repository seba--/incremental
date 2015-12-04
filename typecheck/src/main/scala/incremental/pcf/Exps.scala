package incremental.pcf

import incremental.Node._
import incremental.pcf.PCFCheck._
import incremental.{Context, NodeKind, SyntaxChecking}
import constraints.equality._

/**
 * Created by seba on 13/11/14.
 */

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[Constraint, Result](syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Num extends Exp(simple(Seq(classOf[Integer]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = (TNum, Map())
}
case object Add extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ

    val lcons = EqConstraint(TNum, t1)
    val rcons = EqConstraint(TNum, t2)
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

    context.addConstraints(lcons, rcons)
    context.addConstraintSeq(mcons)

    (TNum, mreqs)
  }
}
case object Mul extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ

    val lcons = EqConstraint(TNum, t1)
    val rcons = EqConstraint(TNum, t2)
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

    context.addConstraints(lcons, rcons)
    context.addConstraintSeq(mcons)

    (TNum, mreqs)
  }
}
case object Var extends Exp(simple(Seq(classOf[Symbol]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val x = lits(0).asInstanceOf[Symbol]
    val X = freshUVar()
    (X, Map(x -> X))
  }
}
case object Abs extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[Type]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    if (lits(0).isInstanceOf[Symbol]) {
      val x = lits(0).asInstanceOf[Symbol]
      val (t, reqs) = kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (lits.size == 2) lits(1).asInstanceOf[Type] else freshUVar()
          (TFun(X, t), reqs)
        case Some(treq) =>
          val otherReqs = reqs - x
          if (lits.size == 2) {
            context.addConstraint(EqConstraint(lits(1).asInstanceOf[Type], treq))
            (TFun(treq, t), otherReqs)
          }
          else
            (TFun(treq, t), otherReqs)
      }
    } else /* if (e.lits(0).isInstanceOf[Seq[_]]) */{
      val xs = lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs) = kids(0).typ

      val Xs = xs map (_ => freshUVar())

      var restReqs = reqs
      var tfun = t
      for (i <- xs.size-1 to 0 by -1) {
        val x = xs(i)
        restReqs.get(x) match {
          case None =>
            val X = freshUVar()
            tfun = TFun(X, tfun)
          case Some(treq) =>
            restReqs = restReqs - x
            tfun = TFun(treq, tfun)
        }
      }

      (tfun, restReqs)
    }
  }
}
case object App extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ

    val X = freshUVar()
    val fcons = EqConstraint(TFun(t2, X), t1)
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

    context.addConstraint(fcons)
    context.addConstraintSeq(mcons)

    (X, mreqs)
  }
}
case object If0 extends Exp(simple(cExp, cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t1, reqs1) = kids(0).typ
    val (t2, reqs2) = kids(1).typ
    val (t3, reqs3) = kids(2).typ

    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)

    val cond = EqConstraint(TNum, t1)
    val body = EqConstraint(t2, t3)

    context.addConstraints(cond, body)
    context.addConstraintSeq(mcons)

    (t2, mreqs)
  }
}
case object Fix extends Exp(simple(cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = {
    val (t, reqs) = kids(0).typ
    val X = freshUVar()
    val fixCons = EqConstraint(t, TFun(X, X))

    context.addConstraint(fixCons)

    (X, reqs)
  }
}
