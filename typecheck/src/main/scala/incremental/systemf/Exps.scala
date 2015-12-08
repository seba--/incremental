package incremental.systemf

import constraints.equality._
import incremental.Node._
import incremental.systemf.SystemFCheck._
import incremental.{Context, SyntaxChecking, NodeKind}

/**
 * Created by seba on 13/11/14.
 */

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[Constraint, Result](syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._

case object Num  extends Exp(simple(Seq(classOf[Integer]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = (TNum, emptyReqs, emptyTReqs)
}
case object Add  extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val (t1, reqs1, treqs1) = kids(0).typ
    val (t2, reqs2, treqs2) = kids(1).typ

    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
    val lcons = EqConstraint(TNum, t1)
    val rcons = EqConstraint(TNum, t2)

    context.addConstraints(lcons, rcons)
    context.addConstraintSeq(mcons)

    (TNum, mreqs, treqs1 ++ treqs2)
  }
}
case object Mul  extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val (t1, reqs1, treqs1) = kids(0).typ
    val (t2, reqs2, treqs2) = kids(1).typ

    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
    val lcons = EqConstraint(TNum, t1)
    val rcons = EqConstraint(TNum, t2)

    context.addConstraints(lcons, rcons)
    context.addConstraintSeq(mcons)

    (TNum, mreqs, treqs1 ++ treqs2)
  }
}
case object Var  extends Exp(simple(Seq(classOf[Symbol]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val x = lits(0).asInstanceOf[Symbol]
    val X = freshUVar()
    (X, Map(x -> X), emptyTReqs)
  }
}
case object Abs  extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[PolType]), cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    if (lits(0).isInstanceOf[Symbol]) {
      val x = lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs) = kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (lits.size == 2) lits(1).asInstanceOf[PolType] else freshUVar()
          (TFun(X, t), reqs, treqs ++ X.freeTVars)
        case Some(treq) =>
          val otherReqs = reqs - x
          if (lits.size == 2) {
            val X = lits(1).asInstanceOf[PolType]
            context.addConstraint(EqConstraint(X, treq))
            (TFun(treq, t), otherReqs, treqs ++ X.freeTVars)
          }
          else
            (TFun(treq, t), otherReqs, treqs)
      }
    } else {
      val xs = lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, treqs) = kids(0).typ

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

      (tfun, restReqs, treqs)
    }
  }
}
case object TAbs extends Exp(simple(Seq(classOf[Symbol]), cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val alpha = lits(0).asInstanceOf[Symbol]
    val (t, reqs, treqs) = kids(0).typ
    (TUniv(alpha, t), reqs, treqs - alpha)
  }
}
case object App  extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val (t1, reqs1, treqs1) = kids(0).typ
    val (t2, reqs2, treqs2) = kids(1).typ

    val X = freshUVar()
    val fcons = EqConstraint(TFun(t2, X), t1)
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

    context.addConstraint(fcons)
    context.addConstraintSeq(mcons)

    (X, mreqs, treqs1 ++ treqs2)
  }
}
case object TApp extends Exp(simple(Seq(classOf[PolType]), cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val (t1, reqs1, treqs) = kids(0).typ
    val t = lits(0).asInstanceOf[PolType]

    val Xalpha = freshUVar().x
    val Xbody = freshUVar()
    val Xres = freshUVar()

    val ucons = EqConstraint(UUniv(Xalpha, Xbody), t1)
    val vcons = EqSubstConstraint(Xbody, Xalpha.x, true, t, Xres) // Xbody[Xalpha:=t] == Xres

    context.addConstraints(ucons, vcons)

    (Xres, reqs1, treqs ++ t.freeTVars)
  }
}
case object If0  extends Exp(simple(cExp, cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val (t1, reqs1, treqs1) = kids(0).typ
    val (t2, reqs2, treqs2) = kids(1).typ
    val (t3, reqs3, treqs3) = kids(2).typ

    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)

    val cond = EqConstraint(TNum, t1)
    val body = EqConstraint(t2, t3)

    context.addConstraints(cond, body)
    context.addConstraintSeq(mcons)

    (t2, mreqs, treqs1 ++ treqs2 ++ treqs3)
  }
}
case object Fix  extends Exp(simple(cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val (t, reqs, treqs) = kids(0).typ
    val X = freshUVar()
    context.addConstraint(EqConstraint(t, TFun(X, X)))
    (X, reqs, treqs)
  }
}
