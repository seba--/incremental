package incremental.haskell

import constraints.Statistics
import constraints.equality.CSubst.CSubst
import constraints.equality._
import incremental.haskell.Node._
import incremental.Util
import incremental.haskell.Condition.trueCond
import incremental.haskell._

/**
 * Created by lira on 29/01/18.
 */
abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[Gen, Constraint, CS ] {

  import csFactory._

  type TError = String

  type LIE = Map[Type, Seq[ClassH]] //Constraint on the type

  type Reqs = Map[Symbol, VarReq]

  type TReqs = Set[Symbol]

  type StepResult = ((LIE, Type), Reqs, TReqs, Seq[Constraint])
  type Result = ((LIE, Type), Reqs, TReqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized {e =>
        val ( (lie, t), reqs, treqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
        val cs = subcs addNewConstraints cons
        var newreq = reqs
          for ((x, vareq) <- reqs) {
            vareq.subst(cs.substitution) match {
              case None => newreq -= x
              case Some(newVR) => newreq += x -> newVR
            }
          }
       // val reqs2 = cs.applyPartialSolutionIt[(Symbol,Type),Map[Symbol,VarReq],Type](reqs, p => p._2)
        e.typ = ((lie, cs applyPartialSolution t), newreq, treqs, cs.propagate)
        true
      }
      val ( (lie, t_), reqs, treqs, cs_) = root.typ
      val cs = cs_.tryFinalize
      val typ = t_.subst(cs.substitution)
      //val typcons = t_._1
      var newreq = reqs
      for ((x, vareq) <- reqs) {
        vareq.subst(cs.substitution) match {
          case None => newreq -= x
          case Some(newVR) => newreq += x -> newVR
        }
      }

//      if (!lie.isEmpty)
//      Right(s"Unresolved variable requirements $lie, type $typ, unres ${cs.unsolved}")
      if (!newreq.isEmpty)
        Right(s"Unresolved variable requirements $reqs, type $typ, unres ${cs.unsolved}")
      else if (!treqs.isEmpty)
        Right(s"Unresolved type-variable requirements $treqs, type $typ, unres ${cs.unsolved}")
      else if (!cs.isSolved)
        Right(s"Unresolved constraints ${cs.unsolved}, type $typ")
      else
        Left(typ)
    }
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {

//    case op if op == Add || op == Mul =>
//      val (t1, reqs1, treqs1, _) = e.kids(0).typ
//      val (t2, reqs2, treqs2, _) = e.kids(1).typ
//
//      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
//      val lcons = EqConstraint(t1._2, TNum)
//      val rcons = EqConstraint(t2._2, TNum)
//
//      (t1, mreqs, treqs1 ++ treqs2, mcons :+ lcons :+ rcons)
//
    case op if op == TAdd || op == TMul =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val cons = EqConstraint(t1._2, t2._2)
      val cCons = NotUniv(t1._2, t2._2)

      (t1, mreqs, treqs1 ++ treqs2, mcons :+ cons :+ cCons)

    case Leq =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val cons = EqConstraint(t1._2, t2._2)
      val lie = mergeLIEMaps(t1._1, t2._1)

      ((lie, TBool), mreqs, treqs1 ++ treqs2, mcons :+ cons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshSchemaVar()
      ( (Map(), InstS(X)), Map(x -> VarReq(X)), Set(), Seq())

    case CInt=>
      val t = TVar('a)
      val lie: LIE = Map(t -> Seq(ClassH('Num)))  // class (Eq a, Show a) => Num a
      ((lie, t), Map(), Set(), Seq())
    case CFloat =>
      val t = freshTVar()
      val lie: LIE = Map(t -> Seq(ClassH('Fractional))) // class Num a => Fractional a
      ((lie, t), Map(), Set(), Seq())
    case CDouble =>
      val t = freshTVar()
      val lie: LIE = Map(t -> Seq(ClassH('Fractional)))
      ((lie, t), Map(), Set(), Seq())
    case CRational =>
      val t = freshTVar()
      val lie: LIE = Map(t -> Seq(ClassH('Fractional)))
      ((lie, t), Map(), Set(), Seq())
    case CChar =>
      val lie: LIE = Map()
      ((lie, TChar), Map(), Set(), Seq())
    case CString =>
      val lie: LIE = Map()
      ((lie, ListH(TChar)), Map(), Set(), Seq())


    case GConB =>
      val name = e.lits(0).asInstanceOf[TCon]
      var vreqs = Set[Symbol]()
      var reqs = Seq[Reqs]()
      var newlie : LIE = Map()
      for (i <- 0 until e.kids.seq.size){
        val (t, req, vreq, _) = e.kids.seq(i).typ
        reqs = reqs :+ req
        vreqs = vreqs ++ vreq
        newlie = newlie ++ t._1
      }
      val (cons, resreq) = mergeReqMaps(reqs)
      val lie = mergeLIEMaps(newlie)

      ((lie, name), resreq, vreqs, cons)

    case GConS =>
      val name = e.lits(0).asInstanceOf[TCon]
      var vreqs = Set[Symbol]()
      var reqs = Seq[Reqs]()
      for (i <- 0 until e.kids.seq.size){
        val (t, req, vreq, _) = e.kids.seq(i).typ
        reqs = reqs :+ req
        vreqs = vreqs ++ vreq
      }
      val (cons, resreq) = mergeReqMaps(reqs)
      val lie: LIE = Map()

      ((lie, name), resreq, vreqs, cons)

    case GConC =>
      val name = e.lits(0).asInstanceOf[TCon]
      var vreqs = Set[Symbol]()
      var reqs = Seq[Reqs]()
      for (i <- 0 until e.kids.seq.size){
        val (t, req, vreq, _) = e.kids.seq(i).typ
        reqs = reqs :+ req
        vreqs = vreqs ++ vreq
      }
      val (cons, resreq) = mergeReqMaps(reqs)
      val lie: LIE = Map()

      ((lie, name), resreq, vreqs, cons)

    case Lit =>
      val (t, reqs, treqs, _) = e.kids(0).typ

      (t, reqs, treqs, Seq())

    case PExp =>
      val (t, reqs, treqs, _) = e.kids(0).typ

      (t, reqs, treqs, Seq())

    case TupleExp =>
      val (t, reqs, treqs, _) = e.kids(0).typ
      var creqs = Seq[Reqs]()
      var treq = treqs
      var tuph = List(t._2)
      var lie: LIE = t._1
      for (i <- 1 until e.kids.seq.size){
        val (t, req, vreq, _) = e.kids.seq(i).typ
        tuph = tuph :+ t._2
        lie = lie ++ t._1
        creqs = creqs :+ req
        treq = treq ++ vreq
      }
      val (cons, resreq) = mergeReqMaps(creqs)
      val (rcons, rReq) = mergeReqMaps(resreq, reqs)

      ((lie, TupleH(tuph)), rReq, treqs, cons ++ rcons)

    case LExp =>
      val (t, reqs, treqs, _) = e.kids(0).typ
      var creqs = Seq[Reqs]()
      var treq = treqs
      for (i <- 1 until e.kids.seq.size){
        val (t, req, vreq, _) = e.kids.seq(i).typ
        creqs = creqs :+ req
        treq = treq ++ vreq
      }
      val (cons, resreq) = mergeReqMaps(creqs)
      val (rcons, rReq) = mergeReqMaps(resreq, reqs)

      ((t._1, ListH(t._2)), rReq, treqs, cons ++ rcons)

    case NSome =>
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      (t1, reqs1, treqs1, Seq())

    case ASeqExp =>
      val (t, reqs, treqs, _) = e.kids(0).typ
      var creq = Seq[Reqs]()
      var restreqs = treqs
      var lie: LIE = t._1
      var rescons = Seq[Constraint]()
      if (e.kids.seq.size == 2) {
        e.kids(1).kind match {
          case NNone =>
            rescons = rescons
            creq = creq
            restreqs = restreqs
          case NSome =>
            val (t1, reqs1, treqs1, _) = e.kids(1).typ
            rescons = rescons :+ EqConstraint(t._2, t1._2)
            lie = lie ++ t1._1
            creq = creq :+ reqs1
            restreqs = restreqs ++ treqs1
        }
      }
      else if (e.kids.seq.size == 3) {
        e.kids(1).kind match {
          case NNone =>
            rescons = rescons
            creq = creq
            restreqs = restreqs
          case NSome =>
            val (t1, reqs1, treqs1, _) = e.kids(1).typ
            rescons = rescons :+ EqConstraint(t._2, t1._2)
            lie = lie ++ t1._1
            creq = creq :+ reqs1
            restreqs = restreqs ++ treqs1
        }
        e.kids(2).kind match {
          case NNone =>
            rescons = rescons
             creq = creq
            restreqs = restreqs
          case NSome =>
            val (t2, reqs2, treqs2, _) = e.kids(2).typ
            rescons = rescons :+ EqConstraint(t._2, t2._2)
            lie = lie ++ t2._1
            creq = creq :+ reqs2
            restreqs = restreqs ++ treqs2
         }
      }

      val (cons1, resreq1) = mergeReqMaps(creq)
      val (cons, resreq) = mergeReqMaps(resreq1, reqs)

      ((lie, SeqH(t._2)), resreq, restreqs, cons1 ++ cons ++ rescons)

    case ListComp if (e.kids(0).isInstanceOf[Exp]) =>
      val (t, req, treq, _ ) = e.kids(0).typ
      var reqs = Seq[Reqs](req)
      var lies = Seq[LIE](t._1)
      var tReqs = Set[Symbol]()
      for (i <- 1 until e.kids.seq.size) {
        val (t, req, treq, _) = e.kids(i).typ
        lies = lies :+ t._1
        reqs = reqs :+ req
        tReqs = tReqs ++ treq
      }
      val (cons, mreq) = mergeReqMaps(reqs)
      val mlie = mergeLIEMaps(lies)

      ((mlie, ListH(t._2)), mreq, treq ++ tReqs, cons)

    case Generator =>
      val (tp, reqp, treqp, _) = e.kids(0).typ
      val (t, req, treq, _) = e.kids(1).typ
      val typ = t._2.asInstanceOf[ListH]

      val (cons, mreq) = mergeReqMaps(reqp, req)
      val lie = mergeLIEMaps(tp._1, t._1)

      ((lie, TNone), mreq, treq ++ treqp, cons :+ EqConstraint(tp._2, typ.elem)) // TODO should return nothing => ((lie, None)......)
   // case LocalDec => // TODO like LetStmt

    case Guard =>
      val (t, req, treq, _) = e.kids(0).typ

      ((t._1, TNone), req, treq, Seq(EqConstraint(t._2, TBool)))  // TODO should be empty return nothing (Some[Type] => None)


    case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t, reqs, treqs, _) = e.kids(0).typ

      reqs.get(x) match {
        case None =>
          val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[HaskellType] else freshUVar()
          ((t._1, TFun(X, t._2)), reqs, treqs ++ X.freeTVars, Seq())
        case Some(treq) =>
          if (e.lits.size ==2 ) {
            val X = e.lits(1).asInstanceOf[HaskellType]
            val otherReqs = reqs - x //satisfyReq((x, X), reqs)
            ((t._1, TFun(X, t._2)), otherReqs, treqs ++ X.freeTVars, Seq(EqConstraint(X, treq.varTyp)))
          }
          else{
          val otherReqs = reqs - x // satisfyReq((x, treq.varTyp), reqs)
            val X = freshUVar()
            val typ = treq.varTyp
            ((t._1, TFun(X, t._2)), otherReqs, treqs, Seq(EqConstraint(X, treq.varTyp)))
          }
      }

    case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
      val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
      val (t, reqs, treqs, _) = e.kids(0).typ

      val Xs = xs map (_ => freshUVar())

      var restReqs = reqs
      var tfun = t._2
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

      ((t._1, tfun), restReqs, treqs, Seq())

      case App =>
        val (t1, reqs1, treqs1, _) = e.kids(0).typ
        val (t2, reqs2, treqs2, _) = e.kids(1).typ

        val X = freshUVar()
        val fcons = EqConstraint(TFun(t2._2, X), t1._2)
        val (mcons, mreqs) = mergeReqMaps(reqs1,reqs2)
        val lie = mergeLIEMaps(t1._1, t2._1)

        ((lie, X), mreqs, treqs1 ++ treqs2, mcons :+ fcons)

    case LetPoly =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val (t1, req1, treq1, _) = e.kids(0).typ
      val (t2, req2, treq2, _) = e.kids(1).typ

      var cons = Seq[Constraint]()
      var reqs = req2

      req2.get(x) match {
        case None => cons
        case Some(t) => cons = cons :+ GenConstraint(t.varTyp, t1._2, Map())
          reqs = reqs - x
      }
   //   val cons = GenConstraint(tsvar, t1._2, Map())

      var lie = Seq[LIE]()
      lie = lie :+ t1._1 :+ t2._1
      val (rescons, resreq) = mergeReqMaps(reqs, req1)
      val reslie = mergeLIEMaps(lie)
      println(rescons ++ cons)
      ((reslie, t2._2), resreq, treq1 ++ treq2, cons ++ rescons)


    case Let =>
      val (te, reqsE, treqE,  _) = e.kids.seq.last.typ
      var newreq = reqsE
      var lie : LIE = te._1
      var reqs = Seq[Reqs]()
      var tReqs = treqE
      var rescons = Seq[Constraint]()
      for (j <- 0 until e.kids.seq.size -1) {
        val (t, req, treq, _) = e.kids(j).typ
        reqs = reqs :+ req
        tReqs = tReqs ++ treq
        lie = lie ++ t._1
      }
      val (con1, req1) = mergeReqMaps(reqs)
      var newreq1 = req1
      for ((x, _) <- reqsE) {
        req1.get(x) match {
          case None =>
            newreq1; newreq; rescons
          case Some(req2) =>
            if (req2.varTyp.isGround) {
              val (reqr, cons) = removeReq((x,req2.varTyp), newreq)
              newreq = reqr; newreq1 = newreq1 - x; rescons = rescons ++ cons
            } else {
              newreq1; newreq; rescons
            }
        }
      }
      val (cons, resreq) = mergeReqMaps(newreq1, newreq)
      val reslie = mergeLIEMaps(lie)

      ((reslie, te._2), resreq, tReqs, rescons ++ cons ++ con1)

    case VarDecl =>
      val s = e.lits(0).asInstanceOf[Symbol]
      val (t, req, treq, _) = e.kids(0).typ
      val (cons, mreq) = mergeReqMaps(Map(s -> VarReq(t._2)), req)
      (t, mreq, treq, cons)

    case Case =>
      val (t, req, treq, _) = e.kids(0).typ
      var cons = Seq[Constraint]()
      var resreq = Seq[Reqs]()
      var lie = Seq[LIE]()
      var restreq = treq
      val (ta, reqa, treqa, _) = e.kids(1).kids(0).typ
      val (tb, reqb, treqb, _) = e.kids(1).kids(1).typ
      cons = cons :+ EqConstraint(t._2, ta._2)
      for (i <- 1 until e.kids.seq.size) {
        val (t1, req1, treq1, _) = e.kids(i).kids(0).typ
        val (t2, req2, treq2, _) = e.kids(i).kids(1).typ
        cons = cons :+ EqConstraint(t._2, t1._2) :+ EqConstraint(tb._2, t2._2)
        resreq = resreq :+ req1 :+ req2
        restreq = restreq ++ treq1 ++ treq2
        lie = lie :+ t1._1 :+ t2._1
      }
      resreq = resreq :+ req :+ reqa :+ reqb
      restreq = restreq ++ treqa ++ treqb
      lie = lie :+ t._1 :+ ta._1 :+ tb._1

      val newlie = mergeLIEMaps(lie)
      val (rescons, reqs) = mergeReqMaps(resreq)

      ((newlie, tb._2), reqs, restreq, cons ++ rescons )

    case Alt =>
      val (t1, req1, treq1, _) = e.kids(0).typ
      val (t2, req2, treq2, _) = e.kids(1).typ

      val lie = mergeLIEMaps(t1._1, t2._1)
      val (cons, mreq) = mergeReqMaps(req1, req2)

      ((lie, t2._2), mreq, treq1 ++ treq2, cons )

    case LitP =>
      val (t, req, treq, _) = e.kids(0).typ
      (t, req, treq, Seq())

    case VarP => //TODO see this again
      val s = e.lits(0).asInstanceOf[Symbol]
      var x = Seq[Type]()
      var reqs = Seq[Reqs]()
      var treqs = Set[TReqs]()
      var lie = Seq[LIE]()
      if (e.kids.seq.size == 1) {
        e.kids(0).kind match {
          case NNone => x = Seq(freshUVar())
          case NSome => val (t, req, treq, _) = e.kids(0).typ
            x = Seq(t._2)
            lie = Seq(t._1)
            reqs = Seq(req)
            treqs = Set(treq)
        }
      }
      else x = Seq(freshUVar())

      if (e.kids.seq.size == 0)
        ((Map(), x.head), Map( s -> VarReq(x.head)), Set() , Seq())
      else
        ((lie.head, x.head), reqs.head, treqs.head , Seq())


    case Rhs =>
      val (t, req, treq, _) = e.kids(0).typ
      (t, req, treq, Seq())

    case SomeDecl =>
      val (t, req, treq, _ )= e.kids(0).typ
      (t, req, treq, Seq())

    case If =>
      var lie: LIE = Map()
      val (t1, reqs1, treqs1, _) = e.kids(0).typ
      val (t2, reqs2, treqs2, _) = e.kids(1).typ
      val (t3, reqs3, treqs3, _) = e.kids(2).typ

      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2, reqs3)

      val cond = EqConstraint(TBool, t1._2)
      val body = EqConstraint(t2._2, t3._2)
      lie = t1._1 ++ t2._1 ++ t3._1

      ((lie, t2._2), mreqs, treqs1 ++ treqs2 ++ treqs3, mcons :+ cond :+ body)

    case Do =>
      val (te, reqsE, treqE,  _) = e.kids.seq.last.typ
      var newreq = reqsE
      var lie : LIE = te._1
      var reqs = Seq[Reqs]()
      var tReqs = treqE
      var rescons = Seq[Constraint]()
      for (j <- 0 until e.kids.seq.size -1) {
        val (t, req, treq, _) = e.kids(j).typ
        reqs = reqs :+ req
        tReqs = tReqs ++ treq
        lie = lie ++ t._1
      }
      val (con1, req1) = mergeReqMaps(reqs)
      var newreq1 = req1
      for ((x, _) <- reqsE) {
        req1.get(x) match {
          case None =>
            newreq1; newreq; rescons
          case Some(req2) =>
            if (req2.varTyp.isGround) {
              val (reqr, cons) = removeReq((x,req2.varTyp), newreq)
              newreq = reqr; newreq1 = newreq1 - x; rescons = rescons ++ cons
            } else {
              newreq1; newreq; rescons
            }
        }
      }
      val (cons, resreq) = mergeReqMaps(newreq1, newreq)
      val reslie = mergeLIEMaps(lie)

      ((reslie, te._2), resreq, tReqs, rescons ++ cons ++ con1)

    case ExpStmt =>
      val (t, req, treq, _) = e.kids(0).typ

      (t, req, treq, Seq())

    case PatStmt =>
      val (t1, req1, treq1, _) = e.kids(0).typ
      val (t2, req2, treq2, _) = e.kids(1).typ

      val lie =  mergeLIEMaps(t1._1, t2._1)
      val (mcons, mreq) = mergeReqMaps(req1, req2)

      ((lie, t2._2), mreq, treq1 ++ treq2, mcons)

    case LetStmt =>
      var reqs = Seq[Reqs]()
      var tReqs = Set[Symbol]()
      var lie = Seq[LIE]()
      var types = List[Type]()
      for (i <- 0 until e.kids.seq.size) {
        val (t, req, treq, _) = e.kids(i).typ
        types = types :+ t._2
        lie = lie :+ t._1
        reqs = reqs :+ req
        tReqs = tReqs ++ treq
      }
      val (mcons, mreq) = mergeReqMaps(reqs)
      val mlie = mergeLIEMaps(lie)

      ((mlie, TNone), mreq, tReqs, mcons)  // TODO or it should be a TupleH(types)

//    case InstDec =>
//      val o =  e.lits(0).asInstanceOf[Symbol]
//      val t = e.lits(1).asInstanceOf[PolType]
//      val (t1, reqs1, treq1, _) = e.kids(0).typ
//      val (t2, reqs2,treq2, _) = e.kids(1).typ
//      val (cons, mreq) = mergeReqMaps(reqs1, reqs2)
//
//      val resreq = satisfyReq((o, t), mreq)
//
//      (t2, resreq, treq1 ++ treq2, cons :+ EqConstraint(t, t1))
  }

  private val initL: (LIE) = Map()

  def mergeLIEMaps(lie: LIE, lies: LIE*): LIE = mergeLIEMaps(lie +: lies)

  def mergeLIEMaps(lies: Seq[LIE]): LIE =
    Util.timed(localState -> Statistics.mergeReqsTime) {
      lies.foldLeft[LIE](initL)(_mergeLIEMaps)
    }

  private def _mergeLIEMaps(was: LIE, newLies: LIE) = {
    val wasLies = was
    var mLies = wasLies
    for ((x, r2) <- newLies)
      wasLies.get(x) match {
        case None => mLies += x -> r2
        case Some(r1) =>
          mLies += x -> (r1 ++ r2)
      }
    mLies
  }

  def unifyLIE(lie : LIE, a : Type, b : Type) : LIE = {
    var newlie = lie
    lie.get(a) match {
      case None => newlie
      case Some(lieA) => lie.get(b) match {
        case None => newlie
        case Some(lieB) =>
          newlie = newlie - b
          newlie += a -> (lieA ++ lieB)
      }
    }
    newlie
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
          (r1.varTyp, r2.varTyp) match {
            case (TSchemaVar(x), r2.varTyp) => mcons = InstConstraint(TSchemaVar(x), r2.varTyp, Seq()) +: mcons
            case (r1.varTyp, TSchemaVar(x)) => mcons = InstConstraint(TSchemaVar(x), r1.varTyp, Seq()) +: mcons
            case (TSchema(t, l), r2.varTyp) =>
              var tsv = Seq[UVar]()
              l.map(i => tsv = tsv :+ freshUVar())
              mcons = InstConstraint(TSchema(t, l), r2.varTyp, tsv) +: mcons
            case (r1.varTyp, TSchema(t, l)) =>
              var tsv = Seq[UVar]()
              l.map(i => tsv = tsv :+ freshUVar())
              mcons = InstConstraint(TSchema(t, l), r1.varTyp, tsv) +: mcons
            case (_, _) => mcons = EqConstraint(r1.varTyp, r2.varTyp) +: mcons
          }
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

  def instanstiate(typ: Type) : Type = {
    typ match {
      case UVar(x) => UVar(x)
      case TFun(t1, t2) => TFun(instanstiate(t1), instanstiate(t2))
      case _ => InstS(typ)
    }
  }
}

case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[Gen, Constraint,CS] {
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

