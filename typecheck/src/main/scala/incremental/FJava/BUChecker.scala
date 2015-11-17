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
case class Ctor(params: ListMap[Symbol, CName], superCall: List[Symbol], fieldDefs: ListMap[Symbol, Symbol])
case class Fields(fld: Map[Symbol, Type])
case class Methods(m : Map[Symbol, (Type, List[Type])])

case class ExtendD( ext : Map[Type,Type])

case class ClassReq(extendc: Option[Type] = None, ctorParams: Option[List[Type]] = None, fields: Fields = Fields(Map()), methods: Methods = Methods(Map()), cmethods : Methods = Methods(Map())) {
  def subst(s: CSubst) = ClassReq(extendc.map(_.subst(s)).map(_.subst(s)), ctorParams.map(_.map(_.subst(s))), Fields(fields.fld.mapValues(_.subst(s)).mapValues(_.subst(s))), Methods(methods.m.mapValues {case (ret, args) => (ret.subst(s).subst(s), args.map(_.subst(s)).map(_.subst(s)))}), Methods(cmethods.m.mapValues {case (ret, args) => (ret.subst(s), args.map(_.subst(s)))}))
}//(supertype, Fields, Methods)

case class CR(cr : Map[Type, ClassReq])

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Type, ClassReq]

  type StepResult = (Type, Reqs, CR, Seq[Constraint])

  type TError = String

  type Prg = String

  var extD = Map[Type, Type]()

  var CTable = Map[Type,(Type,  Ctor, Map[Symbol, Type], Map[Symbol, (Type, List[Type])])]()

  type Result = (Type, Reqs, CR, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        var conss = Seq[Constraint]()
        val (t, reqs, creqs, cons) = typecheckStep(e)
        var cr = CR(Map[Type, ClassReq]())
        val  subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeFJavaSubsystem (res.typ._4, ExtendD(extD)))
        val cs = subcs addNewConstraints cons
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        var creqs2 = CR(Map())
        conss = conss ++ cons
        for ((ts, decl) <- creqs.cr) {
          val decl2 = decl.subst(cs.substitution)
          val c = ts.subst(cs.substitution)
          val (ccons, cres) = mergeCReqMaps(creqs2, CR(Map(c -> decl2)))
          creqs2 =cres
          conss = conss ++ ccons
        }
        val cs1 = cs addNewConstraints(conss)

        e.typ = (cs1 applyPartialSolution t, reqs2, creqs2, cs1.propagate)
        true
      }

      val (t_, reqs, creqs, cs_) = root.typ
      val t = t_.subst(cs_.substitution)
      val (cres,ccons) = remove(CTable, creqs)
      val cs1 = cs_.addNewConstraints(ccons).tryFinalize
      var  cr = cres
      for((c, cld) <- cr.cr){
        if (c.isInstanceOf[UCName]) cr = CR(cr.cr -  c)
      }

      if (!reqs.isEmpty)
        Right(s"Unresolved variable requirements $reqs, type $t, unres ${cs1.unsolved}")
      else if (!cr.cr.isEmpty)
        Right(s"Unresolved type-variable requirements $creqs, type $t, unres ${cs1.unsolved}")
      else if (!cs1.isSolved)

        Right(s"Unresolved constraints ${cs1.unsolved}, type $t")
      else
        Left(t)
    }
  }

  def addFieldReq(creqs: CR, t: Type, f: Symbol, U: Type): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), CR(creqs.cr + (t -> ClassReq(fields = Fields(Map(f -> U))))))
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        fields.fld.get(f) match {
          case None =>
            (Seq(), CR(creqs.cr + (t -> ClassReq(sup, ctor, Fields(fields.fld + (f -> U)), methods, cmethods))))
          case Some(t2) =>
            (Seq(Equal(t2, U)), creqs)
        }
    }
  }

  def addFieldDec(creqs: CR, t: Type, f: Symbol, U: Type): (Seq[Constraint], CR) = {
    //if (t == CName('Object))
    //  return (Seq()) TODO shouldn't class reqs be constraints?
    creqs.cr.get(t) match {
      case None =>
        (Seq(), creqs)
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        fields.fld.get(f) match {
          case None =>
            (Seq(), creqs)
          case Some(t2) =>
            val fnew = Fields(fields.fld - f)
            (Seq(Equal(t2, U)), CR(creqs.cr + (t -> ClassReq(sup, ctor, fnew, methods, cmethods))))
        }
    }
  }

  def addMethodReq(creqs: CR, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CR) = {
    creqs.cr.get(t) match {
      case None =>
        (Seq(), CR(creqs.cr + (t -> ClassReq(methods = Methods(Map(m ->(ret, args)))))))
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        methods.m.get(m) match {
          case None =>
            (Seq(), CR(creqs.cr + (t -> ClassReq(sup, ctor, fields, Methods(methods.m + (m -> (ret, args))), cmethods))))
          case Some((ret2, args2)) =>
            (Seq(Equal(ret2, ret), AllEqual(args2, args)), creqs)
        }
    }
  }

  def addCMethodReq(creqs: CR, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CR) = {
    var res = creqs
    if (t == CName('Object)) {
      (Seq(), creqs)
    }
    else {
      creqs.cr.get(t) match {
        case None =>
          val res: CR = CR(creqs.cr - t + (t -> ClassReq(None, None, Fields(Map()), Methods(Map()), Methods(Map(m ->(ret, args))))))
          (Seq(), res)
        case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
          cmethods.m.get(m) match {
            case None =>
              (Seq(), CR(creqs.cr + (t -> ClassReq(sup, ctor, fields, methods, Methods(cmethods.m + (m ->(ret, args)))))))

            case Some((ret2, args2)) =>
              if (args.length == args2.length) {
                val cons = Equal(ret2, ret) +: (args2 zip args).map(p => Equal(p._1, p._2))
                (cons, creqs)
              }
              else
                (Seq(Equal(ret, ret2), Never(AllEqual(args, args2))), creqs)
          }
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
    var res = creqs
    if (t == CName('Object)) {
      (Seq(), creqs)
    }
    else {
      creqs.cr.get(t) match {
        case None =>
          (Seq(), CR(creqs.cr + (t -> ClassReq(ctorParams = Some(params)))))
        case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
          ctor match {
            case None =>
              (Seq(), CR(creqs.cr + (t -> ClassReq(sup, Some(params), fields, methods, cmethods))))
            case Some(params2) =>
              (Seq(AllEqual(params, params2)), creqs)
          }
      }
    }
  }

  def addSupertypeReq(creqs: CR, t: Type, tsuper: Type): (Seq[Constraint], CR) = {
    var res = creqs
    if (tsuper == CName('Object)) {
      (Seq(), creqs)
    }
    else {
      res = CR(res.cr + (tsuper -> ClassReq(None, None, Fields(Map()), Methods(Map()), Methods(Map()))))
    }
    (Seq(), res)
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

      (CName('TNum), mreqs, mCreqs, mcons :+ lcons :+ rcons)

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
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs,_) = e.kids(0).typ

      (c, reqs, creqs, Seq(Subtype(t, c)))

    case DCast =>
      val (t, reqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, creqs, Seq(Subtype(c, t), NotEqual(c, t)))

    case SCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs, _) = e.kids(0).typ

      (t, reqs, creqs, Seq())

    case MethodDec =>
      val (e0, reqs, creqs, _) = e.kids(0).typ

      val retT = e.lits(0).asInstanceOf[CName]
      val m = e.lits(1).asInstanceOf[Symbol]
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]
      var restReqs = reqs
      var cres = creqs
      var cons = Seq[Constraint]()
      val Uc = freshCName()
      val Ud = freshCName()
      cons = cons :+ Subtype(e0, retT)
      for ((x, xC) <- params) {
        reqs.get(x) match {
          case None => restReqs = restReqs
          case Some(typ) =>
            restReqs = restReqs - x
            cons = Equal(xC, typ) +: cons

        }
      }
      cres = CR(cres.cr + (Uc -> ClassReq(Some(Ud), None, Fields(Map()), Methods(Map()), Methods(Map()))) + (Ud -> ClassReq(None, None, Fields(Map()), Methods(Map()), Methods(Map(m -> (retT, params.toMap.valuesIterator.toList))))))
      restReqs.get('this) match {
        case None => restReqs = restReqs
        case Some(typ) =>
          cons = Equal(typ, Uc) +: cons // Subtype(Uc, typ)
          restReqs = restReqs - 'this
      }

      (MethodOK(Uc), restReqs, cres, cons )

    case ClassDec =>
      var CT = Map[Type,(Type,  Ctor, Map[Symbol, Type], Map[Symbol, (Type, List[Type])])]()
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val Ctor(params, superCall, fieldDefs) = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap
      var restCreq = Seq[CR]()
      var cons = Seq[Constraint]()
      var restReqs = Seq[Reqs]()
      var methods = Map[Symbol, (Type, List[Type])]()
      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, cs) = e.kids(i).typ
        restReqs = restReqs :+ req
        restCreq = restCreq :+ creq
        cons =  Equal(c, t.asInstanceOf[MethodOK].in) +: cons
        val retT = e.kids(i).lits(0).asInstanceOf[CName]
        val m = e.kids(i).lits(1).asInstanceOf[Symbol]
        val params = e.kids(i).lits(2).asInstanceOf[Seq[(Symbol, Type)]]
        var par = List[Type]()
        for((t, p) <- params.toMap)
          par = par :+ p
        methods = methods + (m -> (retT, par))
      }
      val (conss, cr) = mergeCReqMaps(restCreq)
      val (rcons, req) = mergeReqMaps(restReqs)
      extD = Map(c -> sup)
      CT =  CT + (c -> (sup,  Ctor(params, superCall, fieldDefs), fields.toMap, methods))
      cons = conss ++ rcons ++ cons
      val (mconsD, crD) = addSupertypeReq(cr, c, sup)
      val (creq2, cons2) = remove( CT, crD)
      cons = cons2 ++ cons
      (c, req, creq2, cons)


    case ProgramM =>
      var CT = Map[Type,(Type,  Ctor, Map[Symbol, Type], Map[Symbol, (Type, List[Type])])]()
      var restCreq = Seq[CR]()
      var cres = Seq[CR]()
      var cress = Seq[CR]()
      var cresss = Seq[CR]()
      var cr = CR(Map())
      var ext = ExtendD(Map())
      var cons = Seq[Constraint]()
      for (i <- 0 until e.kids.seq.size) {
        val (ct, _, creqs, ccons) = e.kids(i).typ
        val c = e.kids(i).lits(0).asInstanceOf[CName]
        val sup = e.kids(i).lits(1).asInstanceOf[CName]
        val Ctor(params, superCall, fieldDefs) = e.kids(i).lits(2).asInstanceOf[Ctor]
        val fields = e.kids(i).lits(3).asInstanceOf[Seq[(Symbol, Type)]]
        var methods = Map[Symbol, (Type, List[Type])]()
        for (j <- 0 until e.kids(i).kids.seq.size) {
          val (c0, res, mcreqs, cs)= e.kids(i).kids.seq(j).typ
          cons = Equal(c, c0.asInstanceOf[MethodOK].in) +: cons
          val retT = e.kids(i).kids(j).lits(0).asInstanceOf[CName]
          val m = e.kids(i).kids(j).lits(1).asInstanceOf[Symbol]
          val params = e.kids(i).kids(j).lits(2).asInstanceOf[Seq[(Symbol, Type)]]
          var par = List[Type]()
          for((t, p) <- params.toMap)
            par = par :+ p
          methods = methods + (m -> (retT, par))
          //  val (mc, mcr) = addCMethodReq(mcreqs, ct, m, params.toMap.valuesIterator.toList, retT)
          //cress = cress :+ mcr

          //          cons = cons ++ mc
        }
        CT =  CT + (c -> (sup,  Ctor(params, superCall, fieldDefs), fields.toMap, methods))
        ext = ExtendD(ext.ext + (c -> sup))

        //   val (kot, kotc) = mergeCReqMaps(cress)
        // cons = cons ++ kot
        // val (kot1, kotc1) = mergeCReqMaps(kotc, creqs)
        cres = cres :+ creqs//kotc1
      }
      for(i <- 0 until cres.size)
      {
        val (ccres, ccons) = remove(CT, cres(i))
        cresss = cresss :+ ccres
        cons = ccons ++ cons
      }
      val (mcons, mcreqs) = mergeCReqMaps(cresss)
      cons = cons ++ mcons

      CTable = CT
      (CName('Object), Map(),mcreqs, cons)

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

  private def remove(CT : Map[Type, (Type, Ctor,  Map[Symbol, Type], Map[Symbol, (Type, List[Type])])], creq : CR) : (CR, Seq[Constraint]) = {
    var fieldD = Map[Symbol, Type]()
    var cr = creq
    var cons = Seq[Constraint]()
    var ct = CT
    for ((c, cld) <- creq.cr) {
      if (c == CName('Object))
        cr = CR(cr.cr - c)
      else {
        var stype = cld.extendc
        var ctor = cld.ctorParams
        var fields = cld.fields
        var methods = cld.methods
        var cmethods = cld.cmethods
        CT.get(c) match {
          case None => cr
          case Some(clsT) =>
            cld.extendc match {
              case None => stype
              case Some(t) =>
                cons = Equal(clsT._1, t)  +: cons

            }
            fieldD = clsT._2.params.dropRight(clsT._2.fieldDefs.size)
            cld.ctorParams match {
              case None => ctor
              case Some(lt) =>
                for (i <- 0 until lt.size){
                  cons = Equal(clsT._2.params.valuesIterator.toList(i),lt(i)) +: cons }
            }
            for ((f, typ) <- cld.fields.fld) {
              fieldD.get(f) match {
                case None => fields = fields
                case Some(typ2) =>
                  cons = Equal(typ2, typ) +: cons
                  fields = Fields(fields.fld - f)
              }
            }
            for ((f, typ) <- cld.fields.fld) {
              clsT._3.get(f) match {
                case None => fields = fields
                case Some(typ2) =>
                  cons = Equal(typ2, typ) +: cons
                  fields = Fields(fields.fld - f)
              }
            }
            for ((m, rt) <- cld.methods.m) {
              clsT._4.get(m) match {
                case None => methods = methods
                case Some(rt2) =>
                  cons = Equal(rt2._1, rt._1) +: cons
                  if(rt2._2.length == rt._2.length) {
                    for (i <- 0 until rt._2.size) {
                      cons = Equal(rt2._2(i), rt._2(i)) +: cons
                    }
                  }
                  else
                    cons = cons :+ Never(Equal(CName('String), CName('TNum)))
                  methods = Methods(methods.m - m)
              }
            }
            for ((m, rt) <- cld.cmethods.m) {
              clsT._4.get(m) match {
                case None => cmethods = Methods(cmethods.m - m)
                case Some(rt2) =>
                  cons =  Subtype(rt2._1, rt._1) +: cons
                  for (i <- 0 until rt._2.size){
                    cons = Equal(rt._2(i), rt2._2(i)) +: cons }
                  cmethods = Methods(cmethods.m - m)
              }
            }
        }

        if (fields.fld.isEmpty && methods.m.isEmpty && CT.exists( _._1 == c) ) //&& cld.cmethods.m.isEmpty)
          cr = CR(cr.cr - c)
      } }

    (cr,cons)
  }


  private val cinit: (Seq[Constraint], CR) = (Seq(), CR(Map()))

  def mergeCCld(cld1: ClassReq, cld2: ClassReq): (Seq[Constraint], ClassReq) = {
    var ctor : Option[List[Type]] = cld1.ctorParams
    val newF = cld2.fields
    val wasF = cld1.fields
    var rF = wasF
    val wasM = cld1.methods
    var mcons = Seq[Constraint]()
    var cldm = wasM
    var cldmc = cld1.cmethods
    (cld1.ctorParams, cld2.ctorParams) match {
      case (None, None) => ctor
      case (None, Some(_)) => ctor = cld2.ctorParams
      case (Some(_), None) => ctor
      case (Some(t1), Some(t2)) => ctor = cld2.ctorParams
        if (t1.size == t2.size) {
          for (i <- 0 until t1.size) {
            mcons =  Equal(t1(i), t2(i)) +:  mcons }
        }
        else
          mcons = mcons :+ Never(Equal(CName('String), CName('TNum)))
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
    var styp : Option[Type] = cld1.extendc
    (cld1.extendc, cld2.extendc) match {
      case (None, None) => styp
      case (None, Some(_)) => styp = cld2.extendc
      case (Some(_), None) => styp
      case (Some(t1), Some(t2)) => styp = cld2.extendc
        mcons = Equal(t1, t2) +: mcons


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
