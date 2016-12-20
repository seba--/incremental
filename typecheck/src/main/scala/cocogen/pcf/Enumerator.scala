package cocogen.pcf

import incremental.NodeKind
import incremental.pcf._

import scala.util.Random

case class Probabilities(leaf: Double, convergence: Double) {
  def step: Probabilities = new Probabilities(Math.pow(leaf, 1+convergence), convergence)
}

class Enumerator {

  val leaves = Seq(
    Num,
    Var
  )

  val kinds = Seq(
    Num,
    Add,
    Mul,
    Var,
    Abs,
    App,
    If0,
    Fix
  )

  val vars: Seq[String] = (0 to 25).toSeq map (x => (x + 97).toChar.toString)

  def selectRandom[T](ts: Seq[T]) = ts(rnd.nextInt(ts.size))

  case class SExp(k: NodeKind, lits: Seq[Any], kids: Seq[SExp]) {
    override def toString = {
      val subs = lits.map(_.toString) ++ kids.map(_.toString)
      val subssep = if (subs.isEmpty) subs else subs.flatMap(s => Seq(", ", s)).tail
      val substring = subssep.foldLeft("")(_+_)
      val typString = "" //if(typ == null) "" else "@{" + typ.asInstanceOf[Tuple3[_,_,_]]._1 "}"
      s"$k$typString($substring)"
    }
  }
  object SExp {
    def apply(k: NodeKind, subs: Any*) = {
      val (kids, lits) = subs.partition(_.isInstanceOf[SExp])
      new SExp(k, lits.asInstanceOf[Seq[Any]], kids.asInstanceOf[Seq[SExp]])
    }
  }

  val rnd = new Random(0)

  def gen(probs: Probabilities): SExp = {
    val leaf = rnd.nextDouble() >= probs.leaf

    val nextProbs = probs.step
    def next() = gen(nextProbs)

    val kind = if (leaf) selectRandom(leaves) else selectRandom(kinds)

    kind match {
      case Num => SExp(Num, rnd.nextInt())
      case Add => SExp(Add, next(), next())
      case Mul => SExp(Mul, next(), next())
      case Var => SExp(Var, selectRandom(vars))
      case Abs => SExp(Abs, selectRandom(vars), next())
      case App => SExp(App, next(), next())
      case If0 => SExp(If0, next(), next(), next())
      case Fix => SExp(Fix, next())
    }
  }

  // main code
  def gen(): SExp = gen(new Probabilities(0.8, 0.2))
}



//package cocogen.pcf
//
//import constraints.Statistics
//import constraints.equality._
//import incremental.{Node_, Util}
//import incremental.Node.Node
//
//import incremental.pcf._
//
//abstract class Enumerator[CS <: ConstraintSystem[CS]] extends EnumeratorBase[CS] {
//  import csFactory._
//
//  type TError = String
//  type Reqs = Map[Symbol, Type]
//
//  type StepResult = (Exp, Type, Reqs, Seq[Constraint])
//  type Result = (Type, Reqs, CS)
//
//  def typecheckImpl(e: Node): Either[Type, TError] = {
//    val root = e.withType[Result]
//
//    Util.timed(localState -> Statistics.typecheckTime) {
//      root.visitUninitialized {e =>
//        val (t, reqs, cons) = typecheckStep(e)
//        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._3)
//        val cs = subcs addNewConstraints cons
//        val reqs2 = cs.applyPartialSolutionIt[(Symbol,Type),Map[Symbol,Type],Type](reqs, p => p._2)
//        e.typ = (cs applyPartialSolution t, reqs2, cs.propagate)
//        true
//      }
//
//      val (t_, reqs, sol_) = root.typ
//      val sol = sol_.tryFinalize
//      val t = t_.subst(sol.substitution)
//
//      if (!reqs.isEmpty)
//        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
//      else if (!sol.isSolved)
//        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
//      else
//        Left(t)
//    }
//  }
//
//  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
//    case Num => (TNum, Map(), Seq())
//    case op if op == Add || op == Mul =>
//      val (t1, reqs1, _) = e.kids(0).typ
//      val (t2, reqs2, _) = e.kids(1).typ
//
//      val lcons = EqConstraint(TNum, t1)
//      val rcons = EqConstraint(TNum, t2)
//      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
//
//      (TNum, mreqs, mcons :+ lcons :+ rcons)
//    case Var =>
//      val x = e.lits(0).asInstanceOf[Symbol]
//      val X = freshUVar()
//      (X, Map(x -> X), Seq())
//    case App =>
//      val (t1, reqs1, _) = e.kids(0).typ
//      val (t2, reqs2, _) = e.kids(1).typ
//
//      val X = freshUVar()
//      val fcons = EqConstraint(TFun(t2, X), t1)
//      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
//
//      (X, mreqs, mcons :+ fcons)
//    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
//      val x = e.lits(0).asInstanceOf[Symbol]
//      val (t, reqs, _) = e.kids(0).typ
//
//      reqs.get(x) match {
//        case None =>
//          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar()
//          (TFun(X, t), reqs, Seq())
//        case Some(treq) =>
//          val otherReqs = reqs - x
//          if (e.lits.size == 2) {
//            (TFun(treq, t), otherReqs, Seq(EqConstraint(e.lits(1).asInstanceOf[Type], treq)))
//          }
//          else
//            (TFun(treq, t), otherReqs, Seq())
//      }
//    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
//      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
//      val (t, reqs, _) = e.kids(0).typ
//
//      val Xs = xs map (_ => freshUVar())
//
//      var restReqs = reqs
//      var tfun = t
//      for (i <- xs.size-1 to 0 by -1) {
//        val x = xs(i)
//        restReqs.get(x) match {
//          case None =>
//            val X = freshUVar()
//            tfun = TFun(X, tfun)
//          case Some(treq) =>
//            restReqs = restReqs - x
//            tfun = TFun(treq, tfun)
//        }
//      }
//
//      (tfun, restReqs, Seq())
//    case If0 =>
//      val (t1, reqs1, _) = e.kids(0).typ
//      val (t2, reqs2, _) = e.kids(1).typ
//      val (t3, reqs3, _) = e.kids(2).typ
//
//      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)
//
//      val cond = EqConstraint(TNum, t1)
//      val body = EqConstraint(t2, t3)
//
//      (t2, mreqs, mcons :+ cond :+ body)
//
//    case Fix =>
//      val (t, reqs, _) = e.kids(0).typ
//      val X = freshUVar()
//      val fixCons = EqConstraint(t, TFun(X, X))
//      (X, reqs, Seq(fixCons))
//  }
//
//
//
//  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())
//
//  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)
//
//  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
//    Util.timed(localState -> Statistics.mergeReqsTime) {
//      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
//    }
//
//  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
//    val wasReqs = was._2
//    var mcons = was._1
//    var mreqs = wasReqs
//    for ((x, r2) <- newReqs)
//      wasReqs.get(x) match {
//        case None => mreqs += x -> r2
//        case Some(r1) =>
//          mcons = EqConstraint(r1, r2) +: mcons
//      }
//    (mcons, mreqs)
//  }
//
//}
//
//case class EnumeratorFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends EnumeratorBaseFactory[CS] {
//  def makeChecker = new Enumerator[CS] {
//    type CSFactory = factory.type
//    implicit val csFactory: CSFactory = factory
//  }
//}