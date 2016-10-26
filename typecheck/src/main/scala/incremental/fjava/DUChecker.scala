package incremental.fjava

/**
 * Created by lirakuci on 10/26/16.
 */
import constraints.Statistics
import constraints.fjava.CSubst.CSubst
import constraints.fjava._
import incremental.Node._
import incremental.Util
import incremental.fjava.ClassReqs
import incremental.fjava.Condition
import incremental.fjava.CtorCReq
import incremental.fjava.ExtCReq
import incremental.fjava.FieldCReq
import incremental.fjava.MethodCReq
import incremental.{Node_, Util}
/**
 * Created by lirakuci on 3/2/15.
 */

import incremental.fjava.Condition.trueCond

trait CTcls[T <: CTcls[T]] {
  def self: T
  val cls: Type
}

case class ExtCT(cls: Type, ext: Type) extends CTcls[ExtCT] {
  def self = this
 // def subst(s: CSubst) =  ExtCT(cls.subst(s), ext.subst(s))
  }
case class CtorCT(cls: Type, args: Seq[Type]) extends CReq[CtorCT] {
  def self = this
 // def subst(s: CSubst) = CtorCReq(cls.subst(s), args.map(_.subst(s)))

}
case class FieldCT(cls: Type, field: Symbol, typ: Type) extends CTcls[FieldCT] {
  def self = this
 // def subst(s: CSubst) = FieldCReq(cls.subst(s), field, typ.subst(s))
}
case class MethodCT(cls: Type, name: Symbol, params: Seq[Type], ret: Type) extends CTcls[MethodCT] {
  def self = this
//  def subst(s: CSubst) = {
//    val cls_ = cls.subst(s)
//    cond.subst(cls_, s) map (MethodCReq(cls_, name, params.map(_.subst(s)), ret.subst(s), _))
//  }
}


case class CT (
                       ext: Set[ExtCT] = Set(),
                       ctorParams: Set[CtorCReq] = Set(),
                       fields: Set[FieldCReq] = Set(),
                       methods: Set[MethodCReq] = Set(),
                       optMethods: Set[MethodCReq] = Set()) {

case class UnboundVariable(x: Symbol, ctx: Map[Symbol, Type]) extends RuntimeException



case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}


abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type Ctx = Map[Symbol, Type]

  type StepResult = (Type, Seq[Constraint], Seq[CS])

  type TError = String

  type Result = (Type, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      try{
        val (t, sol_) = typecheckStep(root, Map(), CT)
        val sol = sol_.tryFinalize

      if (sol.isSolved)
        Left(t.subst(sol.substitution))
      else
        Right(s"Unresolved constraints ${sol.unsolved}, type ${t.subst(sol.substitution)}, subst ${sol.substitution}")
      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      }
    }
  }

  def typecheckStep(e: Node_[Result], ctx: Ctx, ct : CT): StepResult = e.kind match {

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

      var cons = Seq[Constraint]()
      var reqss: Seq[Reqs] = Seq(reqs0)
      var creqss: Seq[ClassReqs] = Seq(creqs0)
      var params = Seq[Type]()

      for (i <- 1 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids(i).typ
        val U = freshCName()
        params = params :+ U
        cons = cons :+ Subtype(ti, U)
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
      }

      val (mreqs, mcons) = mergeReqMaps(reqss)
      val (creqs, cCons) = mergeCReqMaps(creqss)
      val (mcreqs, mcCons) = creqs.merge(MethodCReq(te, m, params, Uret).lift)

      (Uret, mreqs, mcreqs, mcons ++ cCons ++ mcCons ++ cons)

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

      (c, reqs, creqs, Seq(Subtype(c, t), NotEqual(c, t)))

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
      val (creqs2, condCons) = creqs1.merge(MethodCReq(Ud, m, params.map(_._2), retT).liftOpt)

      (MethodOK, restReqs, creqs2, cons ++ extendCons ++ condCons)

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
  //
  //  private def findTransitive(t1 : Type, umap : Map[Type, Type]) : Type = {
  //    umap.get(t1) match {
  //      case (None) => t1
  //      case Some(t2) => findTransitive(t2, umap)
  //    }
  //  }
  //
  //  private def removeNew(CT : Map[Type, CSig], creqs : CR, rcons : Seq[Constraint]) : (CR, Seq[Constraint]) = {
  //    val creqsCr = creqs.cr
  //    var cr = creqsCr
  //    var cons = rcons //Seq[Constraint]()//rcons
  //    var uvarMap = creqs.umap
  //
  //    for ((c, ClassReq(rsup, rctor, rfields, rmethods, rcmethods)) <- creqs.cr)
  //      CT.get(findTransitive(c, uvarMap)) match {
  //        case None =>
  //        case Some(CSig(sup, ctor, fields, methods)) =>
  //          var restReq = ClassReq()
  //
  //          rsup match {
  //            case None =>
  //            case Some(rsupT) =>
  //              if (sup == null)
  //                restReq = restReq.copy(extendc = Some(rsupT))
  //              else
  //                cons = cons :+ Equal(sup, rsupT)
  //              uvarMap += rsupT -> sup
  //          }
  //
  //          rctor match {
  //            case None =>
  //            case Some(ts) =>
  //              cons = cons :+ AllEqual(ctor.params.values.toList, ts)
  //          }
  //
  //          val fieldD = ctor.params.dropRight(ctor.fieldDefs.size)
  //          for ((f, t) <- rfields.fld)
  //            fields.get(f) match {
  //              case Some(ct) => // subclass fields
  //                cons = cons :+ Equal(ct, t)
  //                uvarMap += (t -> ct)
  //              case None =>
  //                fieldD.get(f) match {
  //                  //current class fields
  //                  case None =>
  //                    restReq = restReq.copy(fields = Fields(restReq.fields.fld + (f -> t)))
  //                  case Some(typ2) =>
  //                    cons = cons :+ Equal(typ2, t)
  //                    uvarMap += (t -> typ2)
  //                }
  //            }
  //
  //          for ((m, (rt, ts)) <- rmethods.m)
  //            methods.get(m) match {
  //              case None =>
  //                restReq = restReq.copy(methods = Methods(restReq.methods.m + (m -> (rt, ts))))
  //              case Some((crt, cts)) =>
  //                cons = cons :+ Equal(crt, rt) :+ AllEqual(cts, ts)
  //            }
  //
  //          for ((m, (rt, ts)) <- rcmethods.m)
  //            methods.get(m) match {
  //              case None => // super types of c
  //                // TODO need produce restReq class requirements for yet unknown superclasses
  //                //if (findCMethodDef(c, CT, (m, rt, ts)).isEmpty) restReq = restReq.copy(cmethods = Methods(restReq.cmethods.m + (m -> (rt, ts))))
  //                cons = cons ++ findCMethodDef(c, CT, (m, rt, ts))
  //              // delete requirement
  //              case Some((crt, cts)) =>
  //                cons = cons :+ Equal(crt, rt) :+ AllEqual(cts, ts)
  //            }
  //          if (restReq.isEmpty)
  //            cr = cr - c
  //          else
  //            cr = cr.updated(c, restReq)
  //      }
  //    println(uvarMap)
  //
  //    // println(s"Constranssssssss areee $cons")
  //    (CR(cr, uvarMap), cons)
  //
  //  }
  //  var consRem = Seq[Constraint]()
  //
  //  def removeLoop(CT : Map[Type, CSig], creqs : CR) : (CR, Seq[Constraint]) = {
  //
  //    val (ncr, ncons) = removeNew(CT, creqs, consRem)
  //    consRem = consRem ++ ncons
  //    if (creqs.umap.size == ncr.umap.size)
  //      (ncr, ncons)
  //    else removeLoop(CT, ncr)
  //  }
  //
  //  private def remove(CT : Map[Type, CSig], creqs : CR) : (CR, Seq[Constraint]) = {
  //    val creqsCr = creqs.cr
  //    var cr = creqsCr
  //    var cons = Seq[Constraint]()
  //
  //    for ((c, CSig(sup, ctor, fields, methods)) <- CT)
  //      creqsCr.get(c) match {
  //        case None =>
  //        case Some(ClassReq(rsup, rctor, rfields, rmethods, rcmethods)) =>
  //          var restReq = ClassReq()
  //
  //          rsup match {
  //            case None =>
  //            case Some(rsupT) =>
  //              if (sup == null)
  //                restReq = restReq.copy(extendc = Some(rsupT))
  //              else
  //                cons = cons :+ Equal(sup, rsupT)
  //          }
  //
  //          rctor match {
  //            case None =>
  //            case Some(ts) =>
  //              cons = cons :+ AllEqual(ctor.params.values.toList, ts)
  //          }
  //
  //          val fieldD = ctor.params.dropRight(ctor.fieldDefs.size)
  //          for ((f, t) <- rfields.fld)
  //            fields.get(f) match {
  //              case Some(ct) => // subclass fields
  //                cons = cons :+ Equal(ct, t)
  //              case None =>
  //                fieldD.get(f) match {
  //                  //current class fields
  //                  case None =>
  //                    restReq = restReq.copy(fields = Fields(restReq.fields.fld + (f -> t)))
  //                  case Some(typ2) =>
  //                    cons = cons :+ Equal(typ2, t)
  //                }
  //            }
  //
  //          for ((m, (rt, ts)) <- rmethods.m)
  //            methods.get(m) match {
  //              case None =>
  //                restReq = restReq.copy(methods = Methods(restReq.methods.m + (m -> (rt, ts))))
  //              case Some((crt, cts)) =>
  //                cons = cons :+ Equal(crt, rt) :+ AllEqual(cts, ts)
  //            }
  //
  //          for ((m, (rt, ts)) <- rcmethods.m)
  //            methods.get(m) match {
  //              case None => // super types of c
  //                // TODO need produce restReq class requirements for yet unknown superclasses
  //                //if (findCMethodDef(c, CT, (m, rt, ts)).isEmpty) restReq = restReq.copy(cmethods = Methods(restReq.cmethods.m + (m -> (rt, ts))))
  //                cons = cons ++ findCMethodDef(c, CT, (m, rt, ts))
  //              // delete requirement
  //              case Some((crt, cts)) =>
  //                cons = cons :+ Equal(crt, rt) :+ AllEqual(cts, ts)
  //            }
  //          if (restReq.isEmpty)
  //            cr = cr - c
  //          else
  //            cr = cr.updated(c, restReq)
  //      }
  //
  //    (CR(cr, creqs.umap), cons)
  //  }
  //
  //  def removeF(ct : Map[Type, CSig], creqs : CR, cs : CS, cons : Seq[Constraint]) : (CR, Seq[Constraint]) = {
  //    val (cres, ccons) = creqs.subst(cs)
  //    val (crF, cF) = remove(ct, cres)
  //    val consF = cons ++ cF
  //    val csF = ccons.addNewConstraints(consF)
  //    if (creqs.cr.size == crF.cr.size) {
  //      (crF, consF)}
  //    else removeF(ct, crF, csF, consF)
  //  }
  //
  //  def findSuperTypes(c : Type, CT : Map[Type, CSig], ls : List[Type]) : List[Type] = {
  //    var ext = ls
  //    CT.get(c) match {
  //      case None => ext
  //      case Some(csig) =>
  //        ext = ext :+  csig.extendc
  //        println(c)
  //        findSuperTypes(csig.extendc, CT, ext)
  //    }
  //  }
  //
  //  def findCMethodDef(c : Type, CT : Map[Type, CSig], method : (Symbol, Type, List[Type])) : Seq[Constraint] = {
  //    var cons = Seq[Constraint]()
  //    val newLst = findSuperTypes(c, CT, List())
  //    println(newLst)
  //    var i = 0
  //    var bool = true
  //    while (bool && i < newLst.length) {
  //      CT.get(newLst(i)) match {
  //        case None => cons
  //          i = i + 1
  //        case Some(CSig(sup, ctor, fields, methods)) =>
  //          methods.get(method._1) match {
  //            case None => cons
  //              i = i + 1
  //            case Some((crt, cts)) =>
  //              cons = cons :+ Equal(crt, method._2) :+ AllEqual(cts, method._3)
  //              bool = false
  //          }
  //      }
  //    }
  //    cons
  //  }
  //
  //  private val cinit: (Seq[Constraint], CR) = (Seq(), CR(Map(), Map()))
  //
  //  def mergeCCld(cld1: ClassReq, cld2: ClassReq): (Seq[Constraint], ClassReq, Map[Type, Type]) = {
  //    var umap = Map[Type, Type]()
  //    val newF = cld2.fields
  //    val wasF = cld1.fields
  //    var rF = wasF
  //    val wasM = cld1.methods
  //    var mcons = Seq[Constraint]()
  //    var cldm = wasM
  //    var cldmc = cld1.cmethods
  //
  //    val ctor = (cld1.ctorParams, cld2.ctorParams) match {
  //      case (None, None) => None
  //      case (None, Some(ps)) => Some(ps)
  //      case (Some(ps), None) => Some(ps)
  //      case (Some(t1), Some(t2)) =>
  //        mcons = mcons :+ AllEqual(t1, t2)
  //        Some(t2)
  //    }
  //
  //    for ((f, typ) <- newF.fld){
  //      wasF.fld.get(f) match {
  //        case None => rF = Fields(rF.fld + (f -> typ))
  //        case Some(typ2) =>
  //          rF = Fields(rF.fld + (f -> typ))
  //          mcons =  Equal(typ2, typ) +: mcons
  //      }
  //    }
  //    for ((m, mbody) <- cld2.methods.m) {
  //      wasM.m.get(m) match {
  //        case None => cldm = Methods(cldm.m + (m -> mbody)) // mdoby = return type + list of parameters
  //        case Some(mbody2) =>
  //          mcons = Equal( mbody2._1, mbody._1) +: mcons
  //          if (mbody._2.length == mbody2._2.length)
  //            for (i <- 0 until mbody._2.length) {
  //              mcons = Equal(mbody2._2(i), mbody._2(i)) +: mcons}
  //          else
  //            mcons = mcons :+ Never(Equal(CName('String), CName('TNum)))
  //          cldm = Methods(cldm.m + (m -> mbody))
  //      }
  //    }
  //    for ((m, mbody) <- cld2.cmethods.m) {
  //      cld1.cmethods.m.get(m) match {
  //        case None => cldmc = Methods(cldmc.m + (m -> mbody)) // mdoby = return type + list of parameters
  //        case Some(mbody2) =>
  //          mcons =  Equal(mbody2._1, mbody._1) +: mcons
  //          if (mbody._2.length == mbody2._2.length)
  //            for (i <- 0 until mbody._2.length) {
  //              mcons =  Equal(mbody2._2(i), mbody._2(i)) +: mcons
  //            }
  //          else {
  //            for (i <- 0 until mbody._2.size) {
  //              mcons =  Equal(mbody2._2(i), mbody._2(i)) +: mcons
  //            }
  //          }
  //          cldmc = Methods(cldmc.m + (m -> mbody))
  //      }
  //    }
  //
  //    val styp = (cld1.extendc, cld2.extendc) match {
  //      case (None, None) => None
  //      case (None, Some(st)) => Some(st)
  //      case (Some(st), None) => Some(st)
  //      case (Some(t1), Some(t2)) =>
  //        mcons = mcons :+ Equal(t1, t2)
  //        // umap += t1 -> t2 // TODO do we actually need the cons from the merging --- > Probably NO
  //        Some(t2)
  //    }
  //
  //    (mcons, ClassReq(styp, ctor, rF, cldm, cldmc), umap)
  //  }
  //

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

  //
  //  private def _mergeCReqMaps(was: (Seq[Constraint], CR), newCReqs: CR) = {
  //    val wasCReqs = was._2
  //    var mcons = was._1
  //    var mcreqs = wasCReqs
  //    var umap = mcreqs.umap
  //    for ((t, cld2) <- newCReqs.cr)
  //      wasCReqs.cr.get(t) match {
  //        case None => mcreqs = CR(mcreqs.cr + (t -> cld2), mcreqs.umap)
  //        case Some(cld1) => mcreqs = CR(mcreqs.cr + (t -> mergeCCld(cld1, cld2)._2), mcreqs.umap)
  //          mcons = mergeCCld(cld1, cld2)._1 ++ mcons
  //          umap = umap ++ mergeCCld(cld1, cld2)._3
  //      }
  //    (mcons, mcreqs)
  //  }

}
