package incremental.typeclasses

import constraints.Statistics
import constraints.equality.CSubst.CSubst
import constraints.equality._
import incremental.Node._
import incremental.{Node_, Util}
import incremental.typeclasses.Condition.trueCond

/**
 * Created by lira on 29/01/18.
 */
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String

  type Reqs = Map[Symbol, VarReq]

  type TReqs = Set[Symbol]

  type StepResult = (Type, Map[Symbol,VarReq], TReqs, Seq[Constraint])
  type Result = (Type, Map[Symbol,VarReq], TReqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        val (t, reqs, treqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
        val cs = subcs addNewConstraints cons
        var newreq = reqs
        println(reqs)
          for ((x, vareq) <- reqs) {
            vareq.subst(cs.substitution) match {
              case None => newreq -= x
              case Some(newVR) => newreq += x -> newVR
            }
          }
        println(newreq)
       // val reqs2 = cs.applyPartialSolutionIt[(Symbol,Type),Map[Symbol,VarReq],Type](reqs, p => p._2)
        e.typ = (cs applyPartialSolution t, newreq, treqs, cs.propagate)
        true
      }

      val (t_, reqs, treqs, cs_) = root.typ
      val cs = cs_.tryFinalize
      println(cs)
      val t = t_.subst(cs.substitution)
      var newreq = reqs
      for ((x, vareq) <- reqs) {
        vareq.subst(cs.substitution) match {
          case None => newreq -= x
          case Some(newVR) => newreq += x -> newVR
        }
      }

      if (!newreq.isEmpty)
        Right(s"Unresolved variable requirements $reqs, type $t, unres ${cs.unsolved}")
      else if (!treqs.isEmpty)
        Right(s"Unresolved type-variable requirements $treqs, type $t, unres ${cs.unsolved}")
      else if (!cs.isSolved)
        Right(s"Unresolved constraints ${cs.unsolved}, type $t")
      else
        Left(t)
    }
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {
    case Num => (TNum, Map(), Set(), Seq())
    case CFloat => (TFloat, Map(), Set(), Seq())
    case CDouble => (TDouble, Map(), Set(), Seq())
    case CInt => (TInt, Map(), Set(), Seq())
    case CChar => (TChar, Map(), Set(), Seq())

    case op if op == Add || op == Mul =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val lcons = EqConstraint(t1, TNum)
      val rcons = EqConstraint(t2, TNum)

      (t1, mreqs, treqs1 ++ treqs2, mcons :+ lcons :+ rcons)

    case op if op == TAdd || op == TMul =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val cons = EqConstraint(t1, t2)
      val cCons = NotUniv(t1, t2)

      (t1, mreqs, treqs1 ++ treqs2, mcons :+ cons :+ cCons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshUVar()
      (X, Map(x -> VarReq(X)), Set(), Seq())

    case App =>

      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ

      val X = freshUVar()
      val fcons = EqConstraint(TFun(t2, X), t1)
      val (mcons, mreqs) = mergeReqMaps(reqs1,reqs2)

      (X, mreqs, treqs1 ++ treqs2, mcons :+ fcons)

    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs, _) = e.kids(0).typ
      println(reqs)

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[PolType] else freshUVar()
          (TFun(X, t), reqs, treqs ++ X.freeTVars, Seq())
        case Some(treq) =>
          if (e.lits.size ==2 ) {
            val X = e.lits(1).asInstanceOf[PolType]
            val otherReqs = reqs - x //satisfyReq((x, X), reqs)
            (TFun(treq.varTyp, t), otherReqs, treqs ++ X.freeTVars, Seq(EqConstraint(X, treq.varTyp)))
          }
          else{
          val otherReqs = reqs - x // satisfyReq((x, treq.varTyp), reqs)
            val typ = treq.varTyp
            (TFun(treq.varTyp, t), otherReqs, treqs, Seq())
          }

//        case Some(treq) =>
//          val otherReqs =   reqs - x
//          if (e.lits.size == 2) {
//            val X = e.lits(1).asInstanceOf[PolType]
//            (TFun(treq.varTyp, t), otherReqs, treqs ++ X.freeTVars, Seq(EqConstraint(X, treq.varTyp)))
//          }
//          else
//            (TFun(treq.varTyp, t), otherReqs, treqs, Seq())
      }

    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, treqs, _) = e.kids(0).typ

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
            tfun = TFun(treq.varTyp, tfun)
        }
      }

      (tfun, restReqs, treqs, Seq())

    case If0 =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ
      val (t3, reqs3, treqs3, _) = e.kids(2).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)

      val cond = EqConstraint(TNum, t1)
      val body = EqConstraint(t2, t3)

      (t2, mreqs, treqs1 ++ treqs2 ++ treqs3, mcons :+ cond :+ body)

    case Fix =>
      val (t, reqs, treqs, _) = e.kids(0).typ
      val X = freshUVar()
      (X, reqs, treqs, Seq(EqConstraint(t, TFun(X, X))))

    case TAbs =>
      val alpha = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs, _) = e.kids(0).typ
      (TUniv(alpha, t), reqs, treqs - alpha, Seq())

    case TApp =>
      val (t1, reqs1, treqs, _) = e.kids(0).typ
      val t = e.lits(0).asInstanceOf[PolType]

      val Xalpha = freshUVar().x
      val Xbody = freshUVar()
      val Xres = freshUVar()

      val ucons = EqConstraint(UUniv(Xalpha, Xbody), t1)
      val vcons = EqSubstConstraint(Xbody, Xalpha.x, true, t, Xres) // Xbody[Xalpha:=t] == Xres

      (Xres, reqs1, treqs ++ t.freeTVars, Seq(ucons, vcons))

    case Let =>
      val u =  e.lits(0).asInstanceOf[Symbol]
      val (t1, reqs1, treq1,  _) = e.kids(0).typ
      val (t2, reqs2, treq2,  _) = e.kids(1).typ
      val (cons, mreq) = mergeReqMaps(reqs1, reqs2)
      var rescons = cons
      var otherReqs = mreq
      mreq.get(u) match {
        case None =>
          rescons
        case Some(treq) =>
         // otherReqs = mreq - u
        // rescons :+= EqConstraint(t1, treq.varTyp)
      }
     val (resreq, cCons) = removeReq((u, t1), otherReqs)
      //println(rescons)
     // println(cCons)
      (t2,resreq, treq1 ++ treq2, rescons ++ cCons)

    case Inst =>
      val o =  e.lits(0).asInstanceOf[Symbol]
      val t = e.lits(1).asInstanceOf[PolType]
      val (t1, reqs1, treq1, _) = e.kids(0).typ
      val (t2, reqs2,treq2, _) = e.kids(1).typ
      val (cons, mreq) = mergeReqMaps(reqs1, reqs2)

      val resreq = satisfyReq((o, t), mreq)

      (t2, resreq, treq1 ++ treq2, cons :+ EqConstraint(t, t1))
  }

  private val init: (Seq[Constraint], Reqs) = (Seq(), Map())


  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[Constraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[Constraint], Reqs) =
    Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Seq[Constraint], Reqs)](init)(_mergeReqMaps)
    }

  private def _mergeReqMaps(was: (Seq[Constraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = EqConstraint(r1.varTyp, r2.varTyp) +: mcons
          mreqs += x -> r2.withCond(Condition(r1.cond.not ++ r2.cond.not))
      }
    (mcons, mreqs)
  }

  def satisfyReq(dec: (Symbol, Type), reqs: Map[Symbol, VarReq]): Reqs = {
    var newcrs = reqs
      reqs.get(dec._1) match {
        case None => newcrs
        case Some(r2) =>
          r2.alsoNot(dec._2)
          match {
            case None => newcrs
            case Some(newR) => newcrs += dec._1 -> newR

          }
      }
//    for ((x, r2) <- reqs) {
//      if (dec._1 == x)
//        newcrs += (x -> Some(r2.alsoNot(dec._2.varTyp)))
//      else
//        newcrs
//    }
    newcrs
  }

    def removeReq(dec: (Symbol, Type), reqs: Reqs): (Reqs, Seq[Constraint]) = {
      var cons = Seq[Constraint]()
      var newcrs = reqs
      reqs.get(dec._1) match {
        case None => newcrs
        case Some(r2) =>
          r2.alsoNot(dec._2) match {
            case None =>
              newcrs = newcrs - dec._1
              cons = cons :+ EqConstraint(dec._2, r2.varTyp)
            case Some(newR) =>
              newcrs -= dec._1 //-> newR
              cons = cons :+ EqConstraint(dec._2, r2.varTyp)
          }
      }
      //      for ((x, r2) <- reqs) {
      //        if (dec._1 == x) {
      //          newcrs += x -> r2.withCond(Condition(Set()))
      //          cons = cons :+ EqConstraint(dec._2, r2.varTyp)
      //        }
      //        else
      //          newcrs
      //      }
      (newcrs, cons)
    }
}

case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}

case class VarReq(varTyp: Type, cond: Condition = trueCond) {
  def self = this
 // def canMerge(other: VarReq): Boolean = varS == other.self.varS
  def subst(s: CSubst) = {
    val typ = varTyp.subst(s)
    cond.subst(typ, s) map (VarReq(typ, _))
  }
  def withCond(c: Condition) = copy(cond = c)
  def alsoNot(n: Type): Option[VarReq] = cond.alsoNot(varTyp, n) map (withCond(_))
}

object Condition {
  val trueCond = Condition(Set())
}
case class Condition(not: Set[Type]){
  def subst(newtyp : Type, s: CSubst): Option[Condition] = {
    val newnot = not flatMap { n =>
      val n2 = n.subst(s)
      if (newtyp == n2)
        return None
      else if (newtyp.isGround && n2.isGround) // && cls != n2 (implicit)
        None
      else
        Some(n2)
    }
    Some(Condition(newnot))
  }

  def alsoNot(inst: Type, n: Type): Option[Condition] =
    if (inst == n )
      None
    else
      Some(Condition(not + n))
}

//  def canMerge(other: VarReq): Boolean = varS == other.self.varS
//
//  def merge(sReq: Reqs): (Reqs, Seq[Constraint]) = {
//    val (resreq, cons) = merge(reqs, sReq.reqs)
//    (Reqs(resreq), cons)
//  }
//
//  private def merge[T <: Req[T]](reqs1: Set[T], reqs2: Set[T]): (Set[T], Seq[Constraint]) = {
//    if (reqs1.isEmpty)
//      return (reqs2, Seq())
//    if (reqs2.isEmpty)
//      return (reqs1, Seq())
//    var cons = Seq[Constraint]()
//    val cr = reqs1.flatMap(cr1 =>
//      reqs2.flatMap(cr2 =>
//        if (cr1.canMerge(cr2)) {
//          val reqDiff = cr2.withCond(Condition(cr1.cond.not ++ cr1.cond.not))
//          cons = cons :+ EqConstraint(cr2.varTyp, cr1.varTyp)
//          Seq(reqDiff)
//        }
//        else
//          Seq(cr1, cr2)
//      )
//    )
//    (cr, cons)
//  }
//def satisfyReq(req1: VarReq, setReqs: Set[VarReq], make: Set[VarReq] => Reqs): Reqs = {
//  val newcrs = setReqs flatMap ( req2 =>
//  if (req1.canMerge(req2)) {
//  req2.alsoNot(req1.varTyp)
//}
//  else
//  Some(req2)
//  )
//  (make(newcrs))
//}
//
//  def removeReq(req1: VarReq, setReqs: Set[VarReq], make: Set[VarReq] => Reqs): (Reqs, Seq[Constraint]) = {
//  var cons = Seq[Constraint]()
//  val newcrs = setReqs flatMap ( req2 =>
//  if (req1.canMerge(req2)) {
//  cons = cons :+ EqConstraint(req1.varTyp, req2.varTyp)
//  Some(req2.withCond(Condition(Set())))
//}
//  else
//  Some(req2)
//  )
//  (make(newcrs), cons)
//}

