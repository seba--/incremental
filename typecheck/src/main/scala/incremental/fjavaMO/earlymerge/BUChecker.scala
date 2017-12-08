package incremental.fjavaMO.earlymerge

import constraints.Statistics
import constraints.fjavaMO.CSubst.CSubst
import constraints.fjavaMO._
import constraints.fjavaMO.impl.SolveContinuousSubstCSEarlyMerge
import incremental.fjavaMO._
import incremental.Node._
import incremental.{Node_, Util}


case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}


abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type Reqs = Map[Symbol, Type]

  type StepResult = (Type, Reqs, ClassContext, Seq[Constraint])

  type TError = String

  type Result = (Type, Reqs, ClassContext, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val (t, reqs, cctx, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem(res.typ._4))
        var newctx = cctx
        var newcons = cons
        val csPre = if (e.kind != ClassDec) subcs else {
          // add inheritance to constraint system
          val c = e.lits(0).asInstanceOf[CName]
          val sup = e.lits(1).asInstanceOf[CName]
          val newcctx = cctx.creqs.calcCMinSel(c, cctx.creqs.methods, subcs)
          //calculate minsel for each method and add it to the minselSet of the requirements
          val methods = e.kids.seq
          val (cctxFact4, factCons3) = newcctx.addFacts(methods.map(m =>
            MethodFact(c,
              m.lits(1).asInstanceOf[Symbol],
              m.lits(2).asInstanceOf[Seq[(Symbol, CName)]].map(_._2),
              m.lits(0).asInstanceOf[CName])))
          newctx = cctxFact4
          newcons = cons ++ factCons3
          subcs.extendz(c, sup)
        }

        val (newt, newreqs, newcctx, newcs) = substFix(t, reqs, newctx, newcons, csPre, false)
        e.typ = (newt, newreqs, newcctx, newcs.propagate)
        true
      }

      val (tRoot, reqsRoot, cctxRoot, csRoot) = root.typ
      val (cctxRootObject, objCons) = cctxRoot.addFact(CtorFact(CName('Object), Seq()))

      val (tFinal, reqsFinal, cctxFinal, csFinal) = substFix(tRoot, reqsRoot, cctxRootObject, objCons, csRoot, true)

      if (!reqsFinal.isEmpty)
        Right(s"Unresolved variable requirements $reqsFinal, type $tFinal, unres ${csFinal.unsolved}")
      else if (!cctxFinal.creqs.isEmpty)
        Right(s"Unresolved class requirements ${cctxFinal.creqs}, type $tFinal, unres ${csFinal.unsolved}")
      else if (!csFinal.isSolved)
        Right(s"Unresolved constraints ${csFinal.unsolved}, type $tFinal")
      else
        Left(tFinal)
    }
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshCName()
      (X, Map(x -> X), ClassContext(), Seq())

    case FieldAcc =>
      val f = e.lits(0).asInstanceOf[Symbol] //symbol
      val (t, reqs, cctx, _) = e.kids(0).typ //subsol
      val U = freshCName()
      val (mcctx, cons) = cctx.addRequirement(FieldCReq (t, f, U))
      (U, reqs, mcctx, cons)

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (te, reqs0, cctx0,  _) = e.kids(0).typ
      val Uret = freshCName()

      var cons = Seq[Constraint]()
      var reqss: Seq[Reqs] = Seq(reqs0)
      var cctxs: Seq[ClassContext] = Seq(cctx0)
      var params = Seq[Type]()

      for (i <- (e.kids.seq.size-1) to 1 by -1) {
        val (ti, subreqs, subCctx, _) = e.kids(i).typ
        val U = freshCName()
        params = U +: params
        cons = Subtype(ti, U) +: cons
        reqss = subreqs +: reqss
        cctxs = subCctx +: cctxs
      }

      val (mreqs, mcons) = mergeReqMaps(reqss)
      val (mCctx, cCons) = mergeClassContexts(cctxs)
      val (mcCctx, mcCons) = mCctx.addRequirement(MethodCReq(te, m, params, Uret,  Map(), Set(), Set()))

      (Uret, mreqs, mcCctx, mcons ++ cCons ++ mcCons ++ cons)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]

      var cons = Seq[Constraint]()
      var reqss = Seq[Reqs]()
      var cctx = Seq[ClassContext]()
      var params: List[Type] = Nil

      for (i <- (e.kids.seq.size-1) to 0 by -1) {
        val (ti, subreqs, subCctx, _) = e.kids.seq(i).typ
        val U = freshCName()
        params = U +: params
        cons =  Subtype(ti, U) +: cons
        reqss = subreqs +: reqss
        cctx = subCctx +: cctx
      }

      val (mreqs, mcons) = mergeReqMaps(reqss)
      val (mCctx, cCons) = mergeClassContexts(cctx)
      val (mcCctx, mcCons) = mCctx.addRequirement(CtorCReq(c, params))

      (c, mreqs, mcCctx, mcons ++ cCons ++ mcCons ++ cons)

    case UCast =>
      val (t, reqs, cctx,_) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, cctx, Seq(Subtype(t, c)))

    case DCast =>
      val (t, reqs, cctx, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, cctx, Seq(Subtype(c, t))) //, NotEqual(c, t)))

    case SCast =>
      val (t, reqs, cctx, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (t, reqs, cctx, Seq(NotSubtype(c, t), NotSubtype(t, c), StupidCastWarning(t, c)))

    case MethodDec =>

      val retT = e.lits(0).asInstanceOf[CName] // return type
      val m = e.lits(1).asInstanceOf[Symbol] // method name
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]

      val (bodyT, bodyReqs, bodyCctx, _) = e.kids(0).typ

      val Uc = freshCName() // current class
      val Ud = freshCName() // current super class

      var restReqs = bodyReqs
      var cons = Seq[Constraint]()

      // body type is subtype of declared return type
      cons = Subtype(bodyT, retT) +: cons

      // remove params and 'this from body requirements
      for ((x, xC) <- ('this, Uc) +: params) {
        bodyReqs.get(x) match {
          case None =>
          case Some(typ) =>
            restReqs = restReqs - x
            cons = Equal(xC, typ) +: cons
        }
      }

      val (cctx1, currentCons) = bodyCctx.addCurrentClassRequirement(Uc)
      val (cctx2, extendCons) = cctx1.addRequirements(Seq(
        ExtCReq(Uc, Ud),
        MethodCReq(Ud, m, params.map(_._2), retT, Map(), Set(), Set(),  true)))

      (MethodOK, restReqs, cctx2, cons ++ currentCons ++ extendCons)

    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val ctor = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, CName)]].toMap
      val methods = e.kids.seq

      var reqss = Seq[Reqs]()
      var cctxs = Seq[ClassContext]()

      // handle all methods, satisfying current-class reqs
      for (i <- (e.kids.seq.size-1) to 0 by -1) {
        val (t, req, cctx, _) = e.kids(i).typ
        reqss = req +: reqss
        cctxs = cctx +: cctxs
      }

      val (reqs, mrcons) = mergeReqMaps(reqss)

      val (cctx1, mccons) = mergeClassContexts(cctxs)
      val (cctx2, currentCons) = cctx1.satisfyCurrentClass(c)

      // constructor initializes all local fields
      val fieldInitCons = AllEqual(fields.values.toList, ctor.fields.values.toList)
      // constructor provides correct arguments to super constructor
      val (cctx3, supCons) = cctx2.addRequirement(CtorCReq(sup, ctor.superParams.values.toList))

      // add facts
      val (cctxFact1, factCons1) = cctx3.addFact(CtorFact(c, ctor.allArgTypes))
      val (cctxFact2, factCons2) = cctxFact1.addFacts(fields.map(f => FieldFact(c, f._1, f._2)))
      val cctxFact3 = cctxFact2.addMinselA(c, methods.map(m =>
        MethodFact(c,
          m.lits(1).asInstanceOf[Symbol],
          m.lits(2).asInstanceOf[Seq[(Symbol, CName)]].map(_._2),
          m.lits(0).asInstanceOf[CName])))
//      val (cctxFact4, factCons3) = cctxFact3.addFacts(methods.map(m =>
//        MethodFact(c,
//          m.lits(1).asInstanceOf[Symbol],
//          m.lits(2).asInstanceOf[Seq[(Symbol, CName)]].map(_._2),
//          m.lits(0).asInstanceOf[CName])))
      val (cctxFact4, factCons3) = cctxFact3.addExtFact(c, sup)

      val newcons = (fieldInitCons +: mccons) ++ mrcons ++ currentCons ++ supCons ++ factCons1 ++ factCons2 ++ factCons3 //++ factCons4
      (c, reqs, cctxFact4, newcons)

    case ProgramM =>

      var reqss = Seq[Reqs]()
      var cctxs = Seq[ClassContext]()

      for (i <- (e.kids.seq.size-1) to 0 by -1) {
        val (ct, reqs, cctx, cs) = e.kids(i).typ
        reqss = reqs +: reqss
        cctxs = cctx +: cctxs
      }

      val (mcreqs, mcons) = mergeClassContexts(cctxs)
      val (mreqs, mrcons) = mergeReqMaps(reqss)

//      var satisfyCons = Seq[Constraint]()
//      var restCReqs = mcreqs
//
//      // remove class requirements
//      for (cls <- e.kids.seq.reverseIterator) {
//        val cname = cls.lits(0).asInstanceOf[CName]
//        val sup = cls.lits(1).asInstanceOf[CName]
//        val ctor = cls.lits(2).asInstanceOf[Ctor]
//        val fields = cls.lits(3).asInstanceOf[Seq[(Symbol, CName)]].toMap
//        val methods = cls.kids.seq
//
//        val (creqs1, cons1) = restCReqs.addFact(CtorFact(cname, ctor.allArgTypes))
//        val (creqs2, cons2) = creqs1.addFacts(fields.map(f => FieldFact(cname, f._1, f._2)))
//        val (creqs3, cons3) = creqs2.addFacts(methods.map(m =>
//          MethodFact(
//            cname,
//            m.lits(1).asInstanceOf[Symbol],
//            m.lits(2).asInstanceOf[Seq[(Symbol, CName)]].map(_._2),
//            m.lits(0).asInstanceOf[CName])))
//        val (creqs4, cons4) = creqs3.addFact(ExtendsFact(cname, sup))
//
//        restCReqs = creqs4
//        satisfyCons = satisfyCons ++ cons1 ++ cons2 ++ cons3 ++ cons4
//      }


      (ProgramOK, mreqs, mcreqs, mcons ++ mrcons)

  }


  private val init: (Reqs, Seq[Constraint]) = (Map(), Seq())

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Reqs, Seq[Constraint]) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Reqs, Seq[Constraint]) =
    Util.timed(localState -> Statistics.mergeReqsTime) {
      reqs.foldLeft[(Reqs, Seq[Constraint])](init)(_mergeReqMaps)
    }

  private def _mergeReqMaps(was: (Reqs, Seq[Constraint]), newReqs: Reqs) = {
    val wasReqs = was._1
    var mcons = was._2
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mreqs += x -> r2
          mcons =  Equal(r1, r2) +: mcons
      }
    (mreqs, mcons)
  }


  private val cinit: (ClassContext, Seq[Constraint]) = (ClassContext(), Seq())

  def mergeClassContexts(cctx: ClassContext, cctxs: ClassContext*): (ClassContext, Seq[Constraint]) = mergeClassContexts(cctx +: cctxs)

  def mergeClassContexts(cctxs: Seq[ClassContext]): (ClassContext, Seq[Constraint]) =
    Util.timed(localState -> Statistics.mergeCReqsTime) {
      if (cctxs.isEmpty)
        cinit
      else
        cctxs.tail.foldLeft[(ClassContext, Seq[Constraint])]((cctxs.head, Seq()))(_mergeClassContexts)
    }

  private def _mergeClassContexts(was: (ClassContext, Seq[Constraint]), newCtxs: ClassContext) = {
    val (mcctxs, cons) = was._1.merge(newCtxs)
    (mcctxs, was._2 ++ cons)
  }

  def substFix(t: Type, reqs: Reqs, cctx: ClassContext, cons: Seq[Constraint], csPre: CS, finalize: Boolean): (Type, Reqs, ClassContext, CS) = {
    var newcons = cons
    var newt = t
    var newcs = csPre
    var newcctx = cctx
    var newreqs = reqs
    var lastSubst: CSubst = Map()
    var atLeastOnce = finalize && cons.isEmpty

    while (!newcons.isEmpty || atLeastOnce) {
      atLeastOnce = false
      newcs = newcs.addNewConstraints(newcons)
      if (finalize)
        newcs = newcs.tryFinalize.asInstanceOf[CS]

      if (!newcs.shouldApplySubst || newcs.substitution == lastSubst) {
        newcons = Seq()
      }
      else {
        val newsubst = newcs.substitution
        val (ctx2, cons2) = newcctx.subst(newsubst)
        newcctx = ctx2
        newcons = cons2
        lastSubst = newsubst
      }

      newreqs = newcs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](newreqs, p => p._2)
      newt = newcs.applyPartialSolution(newt)
    }
    if (finalize)
      newcctx = newcctx.finalized
    (newt, newreqs, newcctx, newcs)
  }
}
