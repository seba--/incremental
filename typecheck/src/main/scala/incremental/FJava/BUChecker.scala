package incremental.fjava

import constraints.Statistics
import constraints.fjava.CSubst.CSubst
import constraints.fjava._
import incremental.Node._
import incremental.{Node_, Util}

import scala.collection.immutable.ListMap
/**
 * Created by lirakuci on 3/2/15.
 */


case class FieldName(x: Symbol)
case class Param(x: Symbol)
case class Ctor(params: ListMap[Symbol, CName], superCall: List[Symbol], fieldDefs: ListMap[Symbol, Symbol]) {
  def supCtorParams: List[Type] = params.values.take(superCall.size).toList
}
case class Fields(fld: Map[Symbol, Type])
case class Methods(m : Map[Symbol, (Type, List[Type])])

case class ExtendD( ext : Map[Type,Type])

case class ClassReq(extendc: Option[Type] = None, ctorParams: Option[List[Type]] = None, fields: Fields = Fields(Map()), methods: Methods = Methods(Map()), cmethods : Methods = Methods(Map())) {
  def subst(s: CSubst) = ClassReq(extendc.map(_.subst(s)), ctorParams.map(_.map(_.subst(s))), Fields(fields.fld.mapValues(_.subst(s)).mapValues(_.subst(s))), Methods(methods.m.mapValues {case (ret, args) => (ret.subst(s).subst(s), args.map(_.subst(s)).map(_.subst(s)))}), Methods(cmethods.m.mapValues {case (ret, args) => (ret.subst(s), args.map(_.subst(s)))}))
  def isEmpty = extendc.isEmpty && ctorParams.isEmpty && fields.fld.isEmpty && methods.m.isEmpty && cmethods.m.isEmpty
}

case class CSig(extendc: Type,  ctor: Ctor, fields: Map[Symbol, Type], methods: Map[Symbol, (Type, List[Type])])

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Type, ClassReq]

  type StepResult = (Type, Reqs, CR, Seq[Constraint])

  type TError = String

  type Prg = String

  type Result = (Type, Reqs, CR, CS)

  case class CR(cr : Map[Type, ClassReq]) {
    def subst(cs: CS, isFinal: Boolean = false): (CR, CS) = {
      val s = cs.substitution
      var creqs = Map[Type, ClassReq]()
      var cons = Seq[Constraint]()
      for ((t, creq) <- cr) {
        val t2 = t.subst(s)
        val creq2 = creq.subst(s)
        creqs.get(t2) match {
          case None =>
            creqs = creqs + (t2 -> creq2)
          case Some(creqOther) =>
            val (newcons, mcreq) = mergeCCld(creqOther, creq2)
            creqs = creqs + (t2 -> mcreq)
            cons = cons ++ newcons
        }
      }

      val cs2 = cs.addNewConstraints(cons)
      val cs3 = if (isFinal) cs2.tryFinalize else cs2
      if (s.size == cs3.substitution.size)
        (CR(creqs), cs3)
      else
        CR(creqs).subst(cs3)
    }
  }

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val (t, reqs, creqs, cons) = typecheckStep(e)
        var cr = CR(Map[Type, ClassReq]())
        var subcs = freshConstraintSystem
        if (e.kind == ProgramM)
          subcs
        else subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem (res.typ._4))
        val cs = subcs addNewConstraints cons
        val (creqs2, cs1) = creqs.subst(cs)
        val reqs2 = cs1.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)

        e.typ = (cs1 applyPartialSolution t, reqs2, creqs2, cs1.propagate)
        true
      }

      val (tRoot, reqsRoot, creqsRoot, csRoot) = root.typ
      val (creqsNoObject, ccons) = remove(Map(CName('Object) -> CSig(null, Ctor(ListMap(), List(), ListMap()), Map(), Map())), creqsRoot)

      val csRootFinal = csRoot.addNewConstraints(ccons).tryFinalize
      val (creqsFinal, csFinal) = creqsNoObject.subst(csRootFinal, isFinal = true)

      val tFinal = tRoot.subst(csFinal.substitution)
      val reqsFinal = reqsRoot mapValues (_.subst(csFinal.substitution))

      // TODO don't store finalized values
      root.typ = (tFinal, reqsFinal, creqsFinal, csFinal)

      if (!reqsFinal.isEmpty)
        Right(s"Unresolved variable requirements $reqsRoot, type $tFinal, unres ${csFinal.unsolved}")
      else if (!creqsFinal.cr.isEmpty)
        Right(s"Unresolved type-variable requirements $creqsRoot, type $tFinal, unres ${csFinal.unsolved}")
      else if (!csFinal.isSolved)

        Right(s"Unresolved constraints ${csFinal.unsolved}, type $tFinal")
      else
        Left(tFinal)
    }
  }

  def addFieldReq(creqs: CR, t: Type, f: Symbol, U: Type): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), CR(creqs.cr + (t -> ClassReq(fields = Fields(Map(f -> U))))))
      case Some(cr) =>
        cr.fields.fld.get(f) match {
          case None =>
            (Seq(), CR(creqs.cr + (t -> cr.copy(fields = Fields(cr.fields.fld + (f -> U))))))
          case Some(t2) =>
            (Seq(Equal(t2, U)), creqs)
        }
    }
  }

  def addMethodReq(creqs: CR, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), CR(creqs.cr + (t -> ClassReq(methods = Methods(Map(m ->(ret, args)))))))
      case Some(cr) =>
        cr.methods.m.get(m) match {
          case None =>
            (Seq(), CR(creqs.cr + (t -> cr.copy(methods = Methods(cr.methods.m + (m -> (ret, args)))))))
          case Some((ret2, args2)) =>
            (Seq(Equal(ret2, ret), AllEqual(args2, args)), creqs)
        }
    }
  }

  def addCMethodReq(creqs: CR, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), CR(creqs.cr + (t -> ClassReq(cmethods = Methods(Map(m ->(ret, args)))))))
      case Some(cr) =>
        cr.cmethods.m.get(m) match {
          case None =>
            (Seq(), CR(creqs.cr + (t -> cr.copy(cmethods = Methods(cr.cmethods.m + (m -> (ret, args)))))))
          case Some((ret2, args2)) =>
            (Seq(Equal(ret, ret2), AllEqual(args, args2)), creqs)
        }
    }
  }

  def addMethodDec(creqs: CR, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), creqs)
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        methods.m.get(m) match {
          case None =>
            (Seq(), creqs)
          case Some((ret2, args2)) =>
            if (args.length == args2.length) {
              val cons = Equal(ret2, ret) +: (args2 zip args).map(p => Equal(p._1, p._2))
              val mnew = methods.m - m
              (cons, CR(creqs.cr - t + (t -> ClassReq(sup, ctor, fields, Methods(mnew), cmethods))))
            }
            else
              (Seq(Equal(ret, ret2), Never(AllEqual(args, args2))), creqs)
        }
    }
  }


  def addCtorReq(creqs: CR, t: Type, params: List[Type]): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), CR(creqs.cr + (t -> ClassReq(ctorParams = Some(params)))))
      case Some(cr) =>
        cr.ctorParams match {
          case None =>
            (Seq(), CR(creqs.cr + (t -> cr.copy(ctorParams = Some(params)))))
          case Some(params2) =>
            (Seq(AllEqual(params, params2)), creqs)
        }
    }
  }

  def addExtendsReq(creqs: CR, t: Type, tsuper: Type): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), CR(creqs.cr + (t -> ClassReq(extendc = Some(tsuper)))))
      case Some(cr) =>
        cr.extendc match {
          case None =>
            (Seq(), CR(creqs.cr + (t -> cr.copy(extendc = Some(tsuper)))))
          case Some(tsuper2) =>
            (Seq(Equal(tsuper, tsuper2)), creqs)
        }
    }
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {

    case Num =>
      (CName('TNum), Map(), CR(Map()),Seq())

    case Str =>
      (CName('TString), Map(), CR(Map()), Seq())

    case op if op == Add || op == Mul =>
      val (t1, reqs1, creqs1, _) = e.kids(0).typ
      val (t2, reqs2, creqs2, _) = e.kids(1).typ

      val lcons = Subtype(t1, CName('TNum))
      val rcons = Subtype(t2, CName('TNum))
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val (mcCons, mCreqs) = mergeCReqMaps(creqs1, creqs2)

      (CName('TNum), mreqs, mCreqs, mcons ++ mcCons :+ lcons :+ rcons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshCName()
      (X, Map(x -> X), CR(Map()), Seq()) // Map(X -> cld), needed at some examples

    case FieldAcc =>
      val f = e.lits(0).asInstanceOf[Symbol] //symbol
      val (t, reqs, creqs, _) = e.kids(0).typ //subsol
      val U = freshCName()
      val (cons, mcreqs) = addFieldReq(creqs, t, f, U)
      (U, reqs, mcreqs, cons)

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (te, reqs0, creqs0,  _) = e.kids(0).typ
      val U = freshCName()

      var cons = Seq[Constraint]()
      var reqss: Seq[Reqs] = Seq(reqs0)
      var creqss: Seq[CR] = Seq(creqs0)
      var param = List[Type]()

      for (i <- 1 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids(i).typ
        val Ui = freshCName()
        param = param :+ Ui
        cons = cons :+ Subtype(ti, Ui)
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
      }

      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)
      val (mcCons, mcreqs) = addMethodReq(creqs, te, m, param, U)

      (U, mreqs, mcreqs, mcons ++ cCons ++ mcCons ++ cons)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      val U = freshCName()

      var cons = Seq[Constraint]()
      var reqss = Seq[Reqs]()
      var creqss = Seq[CR]()
      var ctor: List[Type] = Nil

      for (i <- 0 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids.seq(i).typ
        val Ui = freshCName()
        ctor = ctor :+ Ui
        cons =  cons :+ Subtype(ti, Ui)
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
      }

      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)
      val (mcCons, mcreqs) = addCtorReq(creqs, c, ctor)

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
      val (bodyT, bodyReqs, bodyCreqs, _) = e.kids(0).typ

      val retT = e.lits(0).asInstanceOf[CName]
      val m = e.lits(1).asInstanceOf[Symbol]
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]

      val Uc = freshCName() // current class
      val Ud = freshCName() // current super class

      var restReqs = bodyReqs
      var cons = Seq[Constraint]()

      cons = Subtype(bodyT, retT) +: cons
      // remove params from body requirements
      for ((x, xC) <- params) {
        bodyReqs.get(x) match {
          case None =>
          case Some(typ) =>
            restReqs = restReqs - x
            cons = cons :+ Equal(xC, typ)
        }
      }

      // remove this from body requirements
      bodyReqs.get('this) match {
        case None =>
        case Some(typ) =>
          restReqs = restReqs - 'this
          cons = cons :+ Equal(typ, Uc)
      }

      val (extendCons, creqs1) = addExtendsReq(bodyCreqs, Uc, Ud)
      val (condCons, creqs2) = addCMethodReq(creqs1, Ud, m, params.unzip._2.toList, retT)

      (MethodOK(Uc), restReqs, creqs2, cons ++ extendCons ++ condCons)

    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val ctor = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap

      var CT = Map[Type, CSig]()

      var cons = Seq[Constraint]()
      var reqss = Seq[Reqs]()
      var creqss = Seq[CR]()
      var methods = Map[Symbol, (Type, List[Type])]()

      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, _) = e.kids(i).typ
        reqss = reqss :+ req
        creqss = creqss :+ creq
        cons =  cons :+ Equal(c, t.asInstanceOf[MethodOK].in) // TODO this should be a class requirement `currentClass`

        val retT = e.kids(i).lits(0).asInstanceOf[CName]
        val m = e.kids(i).lits(1).asInstanceOf[Symbol]
        val params = e.kids(i).lits(2).asInstanceOf[Seq[(Symbol, Type)]]
        var par = List[Type]()
        for((t, p) <- params.toMap)
          par = par :+ p
        methods = methods + (m -> (retT, par))
      }

      val (mccons, cr) = mergeCReqMaps(creqss)
      val (mrcons, req) = mergeReqMaps(reqss)

      CT =  CT + (c -> CSig(sup,  ctor, fields.toMap, methods))

      val (mconsD, crD) = addCtorReq(cr, sup, ctor.supCtorParams)
      val (creq2, cons2) = remove(CT, crD)

      (c, req, creq2, cons ++ mccons ++ mrcons ++ mconsD ++ cons2)


    case ProgramM =>

      var reqss = Seq[Reqs]()
      var creqss = Seq[CR]()
      var CT = Map[Type, CSig]()

      // collect class table
      for (i <- 0 until e.kids.seq.size) {
        val (ct, reqs, creqs, _) = e.kids(i).typ
        reqss = reqss :+ reqs
        creqss = creqss :+ creqs
        val csig = extractClassSignature(e.kids(i))
        CT =  CT + csig
      }

      var cons = Seq[Constraint]()
      // satisfy requirements based on class table
      val restCreqss = creqss map { creqs =>
        val (restCreqs, ccons) = remove(CT, creqs)
        cons = cons ++ ccons
        restCreqs
      }

      val (mcons, mcreqs) = mergeCReqMaps(restCreqss)
      val (mrcons, mreqs) = mergeReqMaps(reqss)

      var extD = Map[Type, Type]()
      for ((c, csig) <- CT) extD = extD + (c -> csig.extendc)
      val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeFJavaSubsystem (res.typ._4, ExtendD(extD)))
      val cs = subcs addNewConstraints cons

      val csRootFinal = cs.addNewConstraints(cons ++ mcons ++ mrcons).tryFinalize
      val (creqsFinal, csFinal) = mcreqs.subst(csRootFinal, isFinal = true)

     // val (creqsNoO, cconsO) = remove(Map(CName('Object) -> CSig(null, Ctor(ListMap(), List(), ListMap()), Map(), Map())), creqsFinal)
      val (creqsF, consF) = remove(CT, creqsFinal)

      (ProgramOK, mreqs, creqsF, cons ++ consF)

  }


  def extractClassSignature(e: Node_[Result]): (CName, CSig) = {
    val name = e.lits(0).asInstanceOf[CName]
    val sup = e.lits(1).asInstanceOf[CName]
    val ctor = e.lits(2).asInstanceOf[Ctor]
    val fields = e.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap
    val methods = e.kids.seq.map { em =>
      val retT = em.lits(0).asInstanceOf[CName]
      val m = em.lits(1).asInstanceOf[Symbol]
      val paramTypes = em.lits(2).asInstanceOf[Seq[(Symbol, Type)]].unzip._2.toList
      (m -> (retT, paramTypes))
    }.toMap

    (name -> CSig(sup, ctor, fields, methods))
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
          mreqs += x -> r2
          mcons =  Equal(r1, r2) +: mcons
      }
    (mcons, mreqs)
  }

  private def remove(CT : Map[Type, CSig], creqs : CR) : (CR, Seq[Constraint]) = {
    val creqsCr = creqs.cr
    var cr = creqsCr
    var cons = Seq[Constraint]()

    for ((c, CSig(sup, ctor, fields, methods)) <- CT)
      creqsCr.get(c) match {
        case None =>
        case Some(ClassReq(rsup, rctor, rfields, rmethods, rcmethods)) =>
          var restReq = ClassReq()

          rsup match {
            case None =>
            case Some(rsupT) =>
              if (sup == null)
                restReq = restReq.copy(extendc = Some(rsupT))
              else
                cons = cons :+ Equal(sup, rsupT)
          }

          rctor match {
            case None =>
            case Some(ts) =>
              cons = cons :+ AllEqual(ctor.params.values.toList, ts)
          }
          val fieldD = ctor.params.dropRight(ctor.fieldDefs.size)
          var rF = rfields
          for ((f, t) <- rfields.fld) {
            fieldD.get(f) match {
              case None =>
                rF
              case Some(typ2) =>
                rF = Fields(rF.fld - f)
                cons = cons :+ Equal(typ2, t)
            }
          }
          for ((f, t) <- rfields.fld)
            fields.get(f) match {
              case None =>
                rF//restReq = restReq.copy(fields = Fields(restReq.fields.fld + (f -> t)))
              case Some(ct) =>
                rF = Fields(rF.fld - f)
                cons = cons :+ Equal(ct, t)
            }

          for ((m, (rt, ts)) <- rmethods.m)
            methods.get(m) match {
              case None =>
                restReq = restReq.copy(methods = Methods(restReq.methods.m + (m -> (rt, ts))))
              case Some((crt, cts)) =>
                cons = cons :+ Equal(crt, rt) :+ AllEqual(cts, ts)
            }

          for ((m, (rt, ts)) <- rcmethods.m)
            methods.get(m) match {
              case None =>
                // delete requirement
                // TODO need to check overriding for inheritance distance > 2. Example: A extends B, B extends C, and A.m overrides C.m, but B.m is undefined
              case Some((crt, cts)) =>
                cons = cons :+ Equal(crt, rt) :+ AllEqual(cts, ts)
            }
          restReq = restReq.copy(fields = rF)
          if (restReq.isEmpty)
            cr = cr - c
          else
            cr = cr.updated(c, restReq)
      }

    (CR(cr), cons)
  }

  private def removeOLD(CT : Map[Type, CSig], creq : CR) : (CR, Seq[Constraint]) = {
    var fieldD = Map[Symbol, Type]()
    var cr = creq
    var cons = Seq[Constraint]()
    for ((c, cld) <- creq.cr) {
      var stype = cld.extendc
      var ctor = cld.ctorParams
      var fields = cld.fields
      var methods = cld.methods
      var cmethods = cld.cmethods
      CT.get(c) match {
        case None => cr
        case Some(csig) =>
          cld.extendc match {
            case None =>
            case Some(t) =>
              cons = Equal(csig.extendc, t)  +: cons

          }
          fieldD = csig.ctor.params.dropRight(csig.ctor.fieldDefs.size)
          cld.ctorParams match {
            case None =>
            case Some(lt) =>
              for (i <- 0 until lt.size){
                cons = Equal(csig.ctor.params.valuesIterator.toList(i),lt(i)) +: cons }
          }
          for ((f, typ) <- cld.fields.fld) {
            fieldD.get(f) match {
              case None =>
              case Some(typ2) =>
                cons = Equal(typ2, typ) +: cons
                fields = Fields(fields.fld - f)
            }
          }
          for ((f, typ) <- cld.fields.fld) {
            csig.fields.get(f) match {
              case None => fields = fields
              case Some(typ2) =>
                cons = Equal(typ2, typ) +: cons
                fields = Fields(fields.fld - f)
            }
          }
          for ((m, (rt, argTs)) <- cld.methods.m) {
            csig.methods.get(m) match {
              case None => methods = methods
              case Some((rt2, argTs2)) =>
                cons = Equal(rt2, rt) +: AllEqual(argTs2, argTs) +: cons
                methods = Methods(methods.m - m)
            }
          }
          for ((m, rt) <- cld.cmethods.m) {
            csig.methods.get(m) match {
              case None => cmethods = Methods(cmethods.m - m)
              case Some(rt2) =>
                cons =  Subtype(rt2._1, rt._1) +: cons
                for (i <- 0 until rt._2.size){
                  cons = Equal(rt._2(i), rt2._2(i)) +: cons }
                cmethods = Methods(cmethods.m - m)
            }
          }

        if (fields.fld.isEmpty && methods.m.isEmpty && CT.exists( _._1 == c) ) //&& cld.cmethods.m.isEmpty)
          cr = CR(cr.cr - c)
      } }

    (cr,cons)
  }


  private val cinit: (Seq[Constraint], CR) = (Seq(), CR(Map()))

  def mergeCCld(cld1: ClassReq, cld2: ClassReq): (Seq[Constraint], ClassReq) = {
    val newF = cld2.fields
    val wasF = cld1.fields
    var rF = wasF
    val wasM = cld1.methods
    var mcons = Seq[Constraint]()
    var cldm = wasM
    var cldmc = cld1.cmethods

    val ctor = (cld1.ctorParams, cld2.ctorParams) match {
      case (None, None) => None
      case (None, Some(ps)) => Some(ps)
      case (Some(ps), None) => Some(ps)
      case (Some(t1), Some(t2)) =>
        mcons = mcons :+ AllEqual(t1, t2)
        Some(t2)
    }

    for ((f, typ) <- newF.fld){
      wasF.fld.get(f) match {
        case None => rF = Fields(rF.fld + (f -> typ))
        case Some(typ2) =>
          rF = Fields(rF.fld + (f -> typ))
          mcons =  Equal(typ2, typ) +: mcons
      }
    }
    for ((m, mbody) <- cld2.methods.m) {
      wasM.m.get(m) match {
        case None => cldm = Methods(cldm.m + (m -> mbody)) // mdoby = return type + list of parameters
        case Some(mbody2) =>
          mcons = Equal( mbody2._1, mbody._1) +: mcons
          if (mbody._2.length == mbody2._2.length)
            for (i <- 0 until mbody._2.length) {
              mcons = Equal(mbody2._2(i), mbody._2(i)) +: mcons}
          else
            mcons = mcons :+ Never(Equal(CName('String), CName('TNum)))
          cldm = Methods(cldm.m + (m -> mbody))
      }
    }
    for ((m, mbody) <- cld2.cmethods.m) {
      cld1.cmethods.m.get(m) match {
        case None => cldmc = Methods(cldmc.m + (m -> mbody)) // mdoby = return type + list of parameters
        case Some(mbody2) =>
          mcons =  Equal(mbody2._1, mbody._1) +: mcons
          if (mbody._2.length == mbody2._2.length)
            for (i <- 0 until mbody._2.length) {
              mcons =  Equal(mbody2._2(i), mbody._2(i)) +: mcons
            }
          else {
            for (i <- 0 until mbody._2.size) {
              mcons =  Equal(mbody2._2(i), mbody._2(i)) +: mcons
            }
          }
          cldmc = Methods(cldmc.m + (m -> mbody))
      }
    }

    val styp = (cld1.extendc, cld2.extendc) match {
      case (None, None) => None
      case (None, Some(st)) => Some(st)
      case (Some(st), None) => Some(st)
      case (Some(t1), Some(t2)) =>
        mcons = mcons :+ Equal(t1, t2)
        Some(t2)
    }

    (mcons, ClassReq(styp, ctor, rF, cldm, cldmc))
  }


  def mergeCReqMaps(creq: CR, creqs: CR*): (Seq[Constraint], CR) = mergeCReqMaps(creq +: creqs)

  def mergeCReqMaps(creqs: Seq[CR]): (Seq[Constraint], CR) =
    Util.timed(localState -> Statistics.mergeCReqsTime) {
      creqs.foldLeft[(Seq[Constraint], CR)](cinit)(_mergeCReqMaps)
    }

  private def _mergeCReqMaps(was: (Seq[Constraint], CR), newCReqs: CR) = {
    val wasCReqs = was._2
    var mcons = was._1
    var mcreqs = wasCReqs
    for ((t, cld2) <- newCReqs.cr)
      wasCReqs.cr.get(t) match {
        case None => mcreqs = CR(mcreqs.cr + (t -> cld2))
        case Some(cld1) => mcreqs = CR(mcreqs.cr + (t -> mergeCCld(cld1, cld2)._2))
          mcons = mergeCCld(cld1, cld2)._1 ++ mcons
      }
    (mcons, mcreqs)
  }

}


case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}
