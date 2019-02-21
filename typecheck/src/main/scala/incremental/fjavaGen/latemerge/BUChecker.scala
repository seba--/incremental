package incremental.fjavaGen.latemerge

import constraints.Statistics
import constraints.fjavaGen.CSubst.CSubst
import constraints.fjavaGen._
import incremental.Node._
import incremental.fjavaGen._
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

  type BReqs = Map[Type, Type]

  type StepResult = (Type, Reqs, BReqs,  ClassReqs, Seq[Constraint])

  type TError = String

  type Result = (Type, Reqs, BReqs, ClassReqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val (t, reqs, breqs, creqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem(res.typ._5))
        val csPre = subcs addNewConstraints cons
        val cs = if (e.kind != ClassDec) csPre else {
          // add inheritance to constraint system
          val c = e.lits(0).asInstanceOf[CName]
          val bounds = e.lits(1).asInstanceOf[Seq[Type]]
          val sup = e.lits(2).asInstanceOf[CName]
          csPre.extendz(c, sup).tvarBoundAdd(c.params, bounds)
        }
        val creqs2 = if (cs.shouldApplySubst) creqs.subst(cs.substitution) else creqs
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        e.typ = (cs.applyPartialSolution(t), reqs2, breqs, creqs2, cs.propagate)
        true
      }

      val (tRoot, reqsRoot, breqsRoot, creqsRoot, csRoot) = root.typ
      val (creqsNoObject, objCons) = creqsRoot.satisfyCtor(CtorCReq(CName('Object, Seq()), Seq()))

      val sol = csRoot.addNewConstraints(objCons).tryFinalize

      val tFinal = tRoot.subst(sol.substitution)
      val reqsFinal = reqsRoot mapValues (_.subst(sol.substitution))
      val creqsFinal = creqsNoObject.subst(sol.substitution)

      if (!reqsFinal.isEmpty)
        Right(s"Unresolved variable requirements $reqsFinal, type $tFinal, unres ${sol.unsolved}")
      else if (!creqsFinal.isEmpty)
        Right(s"Unresolved class requirements $creqsFinal, type $tFinal, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $tFinal")
      else
        Left(tFinal)
    }
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {

    case Num => (CName('Num, Seq()), Map(), Map(), ClassReqs(), Seq())
    case Add =>
      val (t1, reqs1,breqs1, creqs1, _) = e.kids(0).typ
      val (t2, reqs2,breqs2, creqs2, _) = e.kids(1).typ

      val lcons = Equal(CName('Num, Seq()), t1)
      val rcons = Equal(CName('Num, Seq()), t2)
      val (mreqs, mcons) = mergeReqMaps(reqs1, reqs2)
      val (mbreqs, mconsB) = mergeBReqMaps(breqs1, breqs2)
      val (mcreqs, mconsC) = mergeCReqMaps(creqs1, creqs2)

      (TNum, mreqs, mbreqs, mcreqs, mcons ++ mconsB ++ mconsC :+ lcons :+ rcons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshCName()
      (X, Map(x -> X), Map(), ClassReqs(), Seq())

    case FieldAcc =>
      val f = e.lits(0).asInstanceOf[Symbol] //symbol
      val (t, reqs, breqs, creqs, _) = e.kids(0).typ //subsol
      val U = freshCName()
      val (mcreqs, cons) = creqs.merge(FieldCReq(t, f, U).lift)
      (U, reqs, breqs, mcreqs, cons)

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
   //   val seqV = e.lits.drop(1).asInstanceOf[Seq[Type]]
      val (te, reqs0, breqs0, creqs0,  _) = e.kids(0).typ
      val Uret = freshCName()

      var cons = Seq[Constraint]()
      var reqss: Seq[Reqs] = Seq(reqs0)
      var creqss: Seq[ClassReqs] = Seq(creqs0)
      var breqss: BReqs = breqs0
      var params = Seq[Type]()

      for (i <- 1 until e.kids.seq.size) {
        val (ti, subreqs, subbreqs, subcreqs, _) = e.kids(i).typ
        val U = freshCName()
        params = params :+ U
        cons = cons :+ Subtype(ti, U) // :+ Subtype(ti, seqV(i))
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
      }

      val (mreqs, mcons) = mergeReqMaps(reqss)
      val (creqs, cCons) = mergeCReqMaps(creqss)
      val (mcreqs, mcCons) = creqs.merge(MethodCReq(te, m, params, Uret).lift)

      (Uret, mreqs, breqss, mcreqs, mcons ++ cCons ++ mcCons ++ cons)

      //TODO see the subtype and ctor req again
    case New =>
      val c = e.lits(0).asInstanceOf[CName]

      var cons = Seq[Constraint]()
      var reqss = Seq[Reqs]()
      var creqss = Seq[ClassReqs]()
      var breqss = Seq[BReqs]()
      var params: List[Type] = Nil

      val genparams = c.params

      for (i <- 0 until e.kids.seq.size) {
        val (ti, subreqs, subbreqs, subcreqs, _) = e.kids.seq(i).typ
        val U = freshCName()
        params = params :+ U
        cons =  cons :+ Subtype(ti, U)
        cons =  cons :+ Subtype(ti, genparams(i))

        reqss = reqss :+ subreqs
        breqss = breqss :+ subbreqs
        creqss = creqss :+ subcreqs
      }

      val (mreqs, mcons) = mergeReqMaps(reqss)
      val (mbreqs, mBcons) = mergeBReqMaps(breqss)
      val (creqs, cCons) = mergeCReqMaps(creqss)
      val (mcreqs, mcCons) = creqs.merge(CtorCReq(c, params).lift)
    //  val (mcreqs, mcCons) = creqs.merge(CtorCReq(c, genparams).lift)

      (c, mreqs, mbreqs, mcreqs, mcons ++ cCons ++ cons ++ mBcons ++ mcCons)

    case UCast =>
      val (t, reqs, breqs, creqs,_) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, breqs, creqs, Seq(Subtype(t, c)))

    case DCast =>
      val (t, reqs, breqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, breqs, creqs, Seq(Subtype(c, t))) //, NotEqual(c, t)))

    case SCast =>
      val (t, reqs, breqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (t, reqs, breqs, creqs, Seq(NotSubtype(c, t), NotSubtype(t, c), StupidCastWarning(t, c)))

    case MethodDec =>

      val retT = e.lits(0).asInstanceOf[Type] // return type
      val m = e.lits(1).asInstanceOf[Symbol] // method name
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]

      val (bodyT, bodyReqs, bodyBreqs, bodyCreqs, _) = e.kids(0).typ

      val Uc = freshCName() // current class
      val Ud = freshCName() // current super class

      var restReqs = bodyReqs
      var cons = Seq[Constraint]()

      // body type is subtype of declared return type
      cons = Subtype(bodyT, retT) +: cons

      // remove params and 'this from body requirements
      for ((x, xC) <- params :+ ('this, Uc)) {
        bodyReqs.get(x) match {
          case None =>
          case Some(typ) =>
            restReqs = restReqs - x
            cons = cons :+ Equal(xC, typ)
        }
      }

      val (creqs1, extendCons) = bodyCreqs.copy(currentClass = Some(Uc)).merge(ExtCReq(Uc, Ud).lift)
    //  val (creqs2, condCons) = creqs1.merge(MethodCReq(Ud, m, params.map(_._2), retT).liftOpt)

      (MethodOK, restReqs, bodyBreqs, creqs1, cons ++ extendCons) //++ condCons)

    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val bounds = e.lits(1).asInstanceOf[Seq[Type]]
      val sup = e.lits(2).asInstanceOf[CName]
      val ctor = e.lits(3).asInstanceOf[Ctor]
      val fields = e.lits(4).asInstanceOf[Seq[(Symbol, Type)]].toMap

      var reqss = Seq[Reqs]()
      var creqss = Seq[ClassReqs]()
      var breqss = Seq[BReqs]()
      var currentClassCons = Seq[Constraint]()

      // handle all methods, satisfying current-class reqs
      for (i <- 0 until e.kids.seq.size) {
        val (t, req, breq, creq, _) = e.kids(i).typ
        reqss = reqss :+ req
        breqss = breqss :+ breq
        val (creq2, cons) = creq.satisfyCurrentClass(c)
        creqss = creqss :+ creq2
        currentClassCons = currentClassCons ++ cons
      }

      val (creqs, mccons) = mergeCReqMaps(creqss)
      val (reqs, mrcons) = mergeReqMaps(reqss)
      val (mbreqs, mBcons) = mergeBReqMaps(breqss)

      // constructor initializes all local fields
      val fieldInitCons = AllEqual(fields.values.toList, ctor.fields.values.toList)
      // constructor provides correct arguments to super constructor
      val (creqs2, supCons) = creqs.merge(CtorCReq(sup, ctor.superParams.values.toList).lift)

      (c, reqs, mbreqs, creqs2, mBcons ++ mccons ++ mrcons ++ currentClassCons ++ supCons :+ fieldInitCons)

    case ProgramM =>

      var reqss = Seq[Reqs]()
      var creqss = Seq[ClassReqs]()

      for (i <- 0 until e.kids.seq.size) {
        val (ct, reqs, breqs, creqs, _) = e.kids(i).typ
        reqss = reqss :+ reqs
        creqss = creqss :+ creqs
      }

      val (mcreqs, mcons) = mergeCReqMaps(creqss)
      val (mreqs, mrcons) = mergeReqMaps(reqss)

      var removeCons = Seq[Constraint]()
      var restCReqs = mcreqs

      // remove class requirements
      for (cls <- e.kids.seq.reverseIterator) {
        val cname = cls.lits(0).asInstanceOf[CName]
        val bounds = cls.lits(1).asInstanceOf[Seq[Type]]
        val sup = cls.lits(2).asInstanceOf[CName]
        val ctor = cls.lits(3).asInstanceOf[Ctor]
        val fields = cls.lits(4).asInstanceOf[Seq[(Symbol, Type)]].toMap
        val methods = cls.kids.seq

        val (creqs1, cons1) = restCReqs.satisfyCtor(CtorCReq(cname, ctor.allArgTypes))
        val (creqs2, cons2) = creqs1.many(_.satisfyField, fields map (f => FieldCReq(cname, f._1, f._2)))
        val (creqs3, cons3) = creqs2.many(_.satisfyMethod,
          methods map (m => MethodCReq(
            cname,
            m.lits(1).asInstanceOf[Symbol],
            m.lits(2).asInstanceOf[Seq[(Symbol, Type)]].map(_._2),
            m.lits(0).asInstanceOf[Type])))
        val (creqs4, cons4) = creqs3.satisfyExtends(ExtCReq(cname, sup))

        restCReqs = creqs4
        removeCons = removeCons ++ cons1 ++ cons2 ++ cons3 ++ cons4
      }


      (ProgramOK, mreqs, Map(), restCReqs, mcons ++ mrcons ++ removeCons)

  }

//
//  def extractClassSignature(e: Node_[Result]): (CName, CSig) = {
//    val name = e.lits(0).asInstanceOf[CName]
//    val sup = e.lits(1).asInstanceOf[CName]
//    val ctor = e.lits(2).asInstanceOf[Ctor]
//    val fields = e.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap
//    val methods = e.kids.seq.map { em =>
//      val retT = em.lits(0).asInstanceOf[CName]
//      val m = em.lits(1).asInstanceOf[Symbol]
//      val paramTypes = em.lits(2).asInstanceOf[Seq[(Symbol, Type)]].unzip._2.toList
//      (m -> (retT, paramTypes))
//    }.toMap
//
//    (name -> CSig(sup, ctor, fields, methods))
//  }
//

  private val binit: (BReqs, Seq[Constraint]) = (Map(), Seq())

  def mergeBReqMaps(breq: BReqs, breqs: BReqs*): (BReqs, Seq[Constraint]) = mergeBReqMaps(breq +: breqs)

  def mergeBReqMaps(breqs: Seq[BReqs]): (BReqs, Seq[Constraint]) =
    Util.timed(localState -> Statistics.mergeReqsTime) {
      breqs.foldLeft[(BReqs, Seq[Constraint])](binit)(_mergeBReqMaps)
    }

  private def _mergeBReqMaps(was: (BReqs, Seq[Constraint]), newBReqs: BReqs) = {
    val wasBReqs = was._1
    var mcons = was._2
    var mreqs = wasBReqs
    for ((tx, r2) <- newBReqs)
      wasBReqs.get(tx) match {
        case None => mreqs += tx -> r2
        case Some(r1) =>
          mreqs += tx -> r2
          mcons =  Equal(r1, r2) +: mcons
      }
    (mreqs, mcons)
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

  private val cinit: (ClassReqs, Seq[Constraint]) = (ClassReqs(), Seq())

  def mergeCReqMaps(creq: ClassReqs, creqs: ClassReqs*): (ClassReqs, Seq[Constraint]) = mergeCReqMaps(creq +: creqs)

  def mergeCReqMaps(creqs: Seq[ClassReqs]): (ClassReqs, Seq[Constraint]) =
    Util.timed(localState -> Statistics.mergeCReqsTime) {
      creqs.foldLeft[(ClassReqs, Seq[Constraint])](cinit)(_mergeCReqMaps)
    }

  private def _mergeCReqMaps(was: (ClassReqs, Seq[Constraint]), newCReqs: ClassReqs) = {
    val (mcreqs, cons) = was._1.merge(newCReqs)
    (mcreqs, was._2 ++ cons)
  }

}
