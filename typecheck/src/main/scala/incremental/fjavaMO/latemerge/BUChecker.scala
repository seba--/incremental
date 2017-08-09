package incremental.fjavaMO.latemerge

import constraints.Statistics
import constraints.fjavaMO.CSubst.CSubst
import constraints.fjavaMO._
import incremental.Node._
import incremental.fjavaMO._
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

  type StepResult = (Type, Reqs, ClassReqs, Seq[Constraint])

  type TError = String

  type Result = (Type, Reqs, ClassReqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val (t, reqs, creqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem(res.typ._4))
        val csPre = subcs addNewConstraints cons
        val cs = if (e.kind != ClassDec) csPre else {
          // add inheritance to constraint system
          val c = e.lits(0).asInstanceOf[CName]
          val sup = e.lits(1).asInstanceOf[CName]
          csPre.extendz(c, sup)
        }
        val creqs2 = if (cs.shouldApplySubst) creqs.subst(cs.substitution) else creqs
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        e.typ = (cs.applyPartialSolution(t), reqs2, creqs2, cs.propagate)
        true
      }

      val (tRoot, reqsRoot, creqsRoot, csRoot) = root.typ
      val (creqsNoObject, objCons) = creqsRoot.satisfyCtor(CtorCReq(CName('Object), Seq()))

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

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshCName()
      (X, Map(x -> X), ClassReqs(), Seq())

    case FieldAcc =>
      val f = e.lits(0).asInstanceOf[Symbol] //symbol
      val (t, reqs, creqs, _) = e.kids(0).typ //subsol
      val U = freshCName()
      val (mcreqs, cons) = creqs.merge(FieldCReq(t, f, U).lift)
      (U, reqs, mcreqs, cons)

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (te, reqs0, creqs0,  _) = e.kids(0).typ
      val Uret = freshCName()

     // var cons = Seq[Constraint]()
      var reqss: Seq[Reqs] = Seq(reqs0)
      var creqss: Seq[ClassReqs] = Seq(creqs0)
      var paramsT = Seq[Type]()
      var params = Seq[Type]()

      for (i <- 1 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids(i).typ
        val U = freshCName()
        paramsT = paramsT :+ ti
        params = params :+ U
        //cons = cons :+ Subtype(ti, U)
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
      }

      val (mreqs, mcons) = mergeReqMaps(reqss)
      val (creqs, cCons) = mergeCReqMaps(creqss)
      val (mcreqs, mcCons) = creqs.merge(MethodCReq(te, m, params ++ paramsT, Uret).lift)

      (Uret, mreqs, mcreqs, mcons ++ cCons ++ mcCons )//++ cons)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]

      var cons = Seq[Constraint]()
      var reqss = Seq[Reqs]()
      var creqss = Seq[ClassReqs]()
      var params: List[Type] = Nil

      for (i <- 0 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids.seq(i).typ
        val U = freshCName()
        params = params :+ U
        cons =  cons :+ Subtype(ti, U)
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
      }

      val (mreqs, mcons) = mergeReqMaps(reqss)
      val (creqs, cCons) = mergeCReqMaps(creqss)
      val (mcreqs, mcCons) = creqs.merge(CtorCReq(c, params).lift)

      (c, mreqs, mcreqs, mcons ++ cCons ++ mcCons ++ cons)

    case UCast =>
      val (t, reqs, creqs,_) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, creqs, Seq(Subtype(t, c)))

    case DCast =>
      val (t, reqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, creqs, Seq(Subtype(c, t))) //, NotEqual(c, t)))

    case SCast =>
      val (t, reqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (t, reqs, creqs, Seq(NotSubtype(c, t), NotSubtype(t, c), StupidCastWarning(t, c)))

    case MethodDec =>

      val retT = e.lits(0).asInstanceOf[CName] // return type
      val m = e.lits(1).asInstanceOf[Symbol] // method name
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]

      val (bodyT, bodyReqs, bodyCreqs, _) = e.kids(0).typ

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
      //val (creqs2, condCons) = creqs1.merge(MethodCReq(Ud, m, params.map(_._2), retT).liftOpt)

      (MethodOK, restReqs, creqs1, cons ++ extendCons )// ++ condCons)

    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val ctor = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap

      var reqss = Seq[Reqs]()
      var creqss = Seq[ClassReqs]()
      var currentClassCons = Seq[Constraint]()

      // handle all methods, satisfying current-class reqs
      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, _) = e.kids(i).typ
        reqss = reqss :+ req
        val (creq2, cons) = creq.satisfyCurrentClass(c)
        creqss = creqss :+ creq2
        currentClassCons = currentClassCons ++ cons
      }

      val (creqs, mccons) = mergeCReqMaps(creqss)
      val (reqs, mrcons) = mergeReqMaps(reqss)

      // constructor initializes all local fields
      val fieldInitCons = AllEqual(fields.values.toList, ctor.fields.values.toList)
      // constructor provides correct arguments to super constructor
      val (creqs2, supCons) = creqs.merge(CtorCReq(sup, ctor.superParams.values.toList).lift)

      (c, reqs, creqs2, mccons ++ mrcons ++ currentClassCons ++ supCons :+ fieldInitCons)

    case ProgramM =>

      var reqss = Seq[Reqs]()
      var creqss = Seq[ClassReqs]()

      for (i <- 0 until e.kids.seq.size) {
        val (ct, reqs, creqs, _) = e.kids(i).typ
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
        val sup = cls.lits(1).asInstanceOf[CName]
        val ctor = cls.lits(2).asInstanceOf[Ctor]
        val fields = cls.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap
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


      (ProgramOK, mreqs, restCReqs, mcons ++ mrcons ++ removeCons)

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
