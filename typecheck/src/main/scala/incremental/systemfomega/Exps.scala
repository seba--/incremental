package incremental.systemfomega

import constraints.normequality.Type._
import constraints.normequality._
import incremental.Node._
import incremental.systemfomega.OmegaCheck._
import incremental.{Context, Node, SyntaxChecking, NodeKind}

/**
 * Created by seba on 13/11/14.
 */

abstract class Exp(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind[Constraint, Result](syntaxcheck)
object Exp {
  val cExp = classOf[Exp]
}
import Exp._


case object Num  extends Exp(simple(Seq(classOf[Integer]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]): Result = ExpResult(TNum(), emptyReqs, emptyTReqs)
}
case object Add  extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val ExpResult(t1, reqs1, treqs1) = kids(0).typ
    val ExpResult(t2, reqs2, treqs2) = kids(1).typ

    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
    val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)
    val lcons = EqConstraint(TNum(), t1)
    val rcons = EqConstraint(TNum(), t2)

    context.addConstraints(lcons, rcons)
    context.addConstraintSeq(mcons)
    context.addConstraintSeq(mtcons)

    ExpResult(TNum(), mreqs, mtreqs)
  }
}
case object Mul  extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val ExpResult(t1, reqs1, treqs1) = kids(0).typ
    val ExpResult(t2, reqs2, treqs2) = kids(1).typ

    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
    val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)
    val lcons = EqConstraint(TNum(), t1)
    val rcons = EqConstraint(TNum(), t2)

    context.addConstraints(lcons, rcons)
    context.addConstraintSeq(mcons)
    context.addConstraintSeq(mtcons)

    ExpResult(TNum(), mreqs, mtreqs)
  }
}
case object Var  extends Exp(simple(Seq(classOf[Symbol]))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val x = lits(0).asInstanceOf[Symbol]
    val X = freshUVar()
    ExpResult(X, Map(x -> X), emptyTReqs)
  }
}
case object Abs  extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol]), cType, cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), cExp))){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    if (lits(0).isInstanceOf[Symbol]) {
      val x = lits(0).asInstanceOf[Symbol]

      if (kids(0).kind.isInstanceOf[Type.Kind]) {
        val TypeResult(k, treqs1) = kids(0).typ
        val T = Type.from(kids(0))
        val ExpResult(t, reqs, treqs2) = kids(1).typ

        val kcons = EqKindConstraint(k, KStar)
        val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)

        context.addConstraint(kcons)
        context.addConstraintSeq(mtcons)

        reqs.get(x) match {
          case None => ExpResult(TFun(T, t), reqs, mtreqs)
          case Some(treq) =>
            context.addConstraint(EqConstraint(T, treq))
            ExpResult(TFun(T, t), reqs - x, mtreqs)
        }
      }
      else {
        val ExpResult(t, reqs, treqs) = kids(0).typ

        reqs.get(x) match {
          case None => ExpResult(TFun(freshUVar(), t), reqs, treqs)
          case Some(treq) => ExpResult(TFun(treq, t), reqs - x, treqs)
        }
      }
    } else {
      val xs = lits(0).asInstanceOf[Seq[Symbol]]
      val ExpResult(t, reqs, treqs) = kids(0).typ

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

      ExpResult(tfun, restReqs, treqs)
    }
  }
}
case object TAbs extends Exp(simple(Seq(classOf[Symbol]), cExp) orElse simple(Seq(classOf[Symbol], classOf[Kind]), cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val alpha = lits(0).asInstanceOf[Symbol]
    val k = if (lits.size == 2) lits(1).asInstanceOf[Kind] else freshKUVar()
    val ExpResult(t, reqs, treqs) = kids(0).typ

    treqs.get(alpha) match {
      case None => ExpResult(TUniv(alpha, Some(k), t), reqs, treqs)
      case Some(k2) =>
        context.addConstraint(EqKindConstraint(k, k2))
        ExpResult(TUniv(alpha, Some(k), t), reqs, treqs - alpha)
    }
  }
}
case object App  extends Exp(simple(cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val ExpResult(t1, reqs1, treqs1) = kids(0).typ
    val ExpResult(t2, reqs2, treqs2) = kids(1).typ

    val X = freshUVar()
    val fcons = EqConstraint(TFun(t2, X), t1)
    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
    val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)

    context.addConstraint(fcons)
    context.addConstraintSeq(mcons)
    context.addConstraintSeq(mtcons)

    ExpResult(X, mreqs, mtreqs)
  }
}
case object TApp extends Exp(simple(cExp, cType)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val ExpResult(t1, reqs1, treqs1) = kids(0).typ
    val TypeResult(k, treqs2) = kids(1).typ
    val t = Type.from(kids(1))

    val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2)

    val Xalpha = freshUVar().x
    val Xbody = freshUVar()
    val Xres = freshUVar()

    val ucons = EqConstraint(UUniv(Xalpha, k, Xbody), t1)
    val vcons = EqSubstConstraint(Xbody, Xalpha.x, true, t, Xres) // Xbody[Xalpha:=t] == Xres

    context.addConstraints(ucons, vcons)
    context.addConstraintSeq(mtcons)

    ExpResult(Xres, reqs1, mtreqs)
  }
}
case object If0  extends Exp(simple(cExp, cExp, cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val ExpResult(t1, reqs1, treqs1) = kids(0).typ
    val ExpResult(t2, reqs2, treqs2) = kids(1).typ
    val ExpResult(t3, reqs3, treqs3) = kids(2).typ

    val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)
    val (mtcons, mtreqs) = mergeTReqMaps(treqs1, treqs2, treqs3)

    val cond = EqConstraint(TNum(), t1)
    val body = EqConstraint(t2, t3)

    context.addConstraints(cond, body)
    context.addConstraintSeq(mcons)
    context.addConstraintSeq(mtcons)

    ExpResult(t2, mreqs, mtreqs)
  }
}
case object Fix  extends Exp(simple(cExp)){
  def check(lits: Seq[Any], kids: Seq[Kid], context: Context[Constraint]) = {
    val ExpResult(t, reqs, treqs) = kids(0).typ
    val X = freshUVar()
    context.addConstraint(EqConstraint(t, TFun(X, X)))
    ExpResult(X, reqs, treqs)
  }
}
