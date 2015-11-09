package incremental.fjava

import constraints.equality.EqConstraint
import constraints.fjava.CSubst.CSubst
import constraints.{CVar, Statistics}
import constraints.fjava._
import constraints.fjava.impl
import incremental.{NodeKind, Node_, Util}
import incremental.Node._
import scala.collection.immutable.ListMap
/**
 * Created by lirakuci on 3/2/15.
 */


case class FieldName(x: Symbol)
case class Param(x: Symbol)
case class Ctor(params: ListMap[Symbol, CName], superCall: List[Symbol], fieldDefs: ListMap[Symbol, Symbol])
case class ExtendD(ext : Map[Type, Type])
case class Fld(fld: Map[Symbol, Type])
case class M(m : Map[Symbol, (Type, List[Type])])

case class ClassReq(extendc: Option[Type], ctorParams: Option[List[Type]], fields: Fld, methods: M, cmethods : M) {
  def subst(s: CSubst) = ClassReq(extendc.map(_.subst(s)), ctorParams.map(_.map(_.subst(s))), Fld(fields.fld.mapValues(_.subst(s))), M(methods.m.mapValues {case (ret, args) => (ret.subst(s), args.map(_.subst(s)))}), M(cmethods.m.mapValues {case (ret, args) => (ret.subst(s), args.map(_.subst(s)))}))
}//(supertype, Fields, Methods)

case class CR(cr : Map[Type, ClassReq])

abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

 // type Methods = Map[Symbol, (Type, List[Type])]

//  type Fields = Map[Symbol, Type]

  //case class ClassR(extendc: Option[Type], ctorParams: Option[List[Type]], fields: Fields, methods: Methods, cmethods : Methods) {
 //   def subst(s: CSubst) = ClassR(extendc.map(_.subst(s)), ctorParams.map(_.map(_.subst(s))), fields.mapValues(_.subst(s)), methods.mapValues {case (ret, args) => (ret.subst(s), args.map(_.subst(s)))}, cmethods.mapValues {case (ret, args) => (ret.subst(s), args.map(_.subst(s)))})
 // }//(supertype, Fields, Methods)


  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Type, ClassReq]

  type StepResult = (Type, Reqs, CR, Seq[Constraint])

  case class ClassDef(name: CName, superClass: CName, fields: Fields, methods: Methods, ctor: Ctor)
  case class MethodDef(name: Symbol, params: ListMap[Symbol, CName], returnType: CName, body: Node_[Any])

  type TError = String

  type Prg = String

  type Result = (Type, Reqs, CR, CS)
  var s = Map[Type,Type]()
  val extt = ExtendD(s)

  def finalT(t : Type, cs : CS) : Type = {
   val c = t.subst(cs.substitution)
    if (c == CName) c
    else finalT(t.subst(cs.substitution), cs)
  }

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val (t, reqs, creqs, cons) = typecheckStep(e)
        var cr = Map[Type, ClassReq]()
        for (i <- 0 until e.kids.seq.size){
          val (t, req, creq, _) = e.kids(i).typ
              cr = cr ++ creq.cr
        }
        val  subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem (res.typ._4, cr1))
        val cs = subcs addNewConstraints cons
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        var creqs2: CReqs = Map()
        for ((ts, decl) <- creqs.cr) {
          val decl2 = decl.subst(cs.substitution)
        //  val c = finalT(ts, cs)
         // val (mcons, cres) = mergeCReqMaps(creqs2,Map(c -> decl2))
          //creqs2 = cres
          val c = ts.subst(cs.substitution)
          if (c == CName) {
          val (mcons, cres) = mergeCReqMaps(creqs2,Map(c -> decl2))
            creqs2 = cres }
          else {
            val c2 = c.subst(cs.substitution)
            val (mcons, cres) = mergeCReqMaps(creqs2,Map(c2 -> decl2))
            creqs2 = cres
          }
        }
        e.typ = (cs applyPartialSolution t, reqs2, CR(creqs2), cs.propagate)
        true
      }

      val (t_, reqs, creqs, cs_) = root.typ
      val cs = cs_.tryFinalize
      val t = t_.subst(cs.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved variable requirements $reqs, type $t, unres ${cs.unsolved}")
      else if (!creqs.cr.isEmpty)
        Right(s"Unresolved type-variable requirements $creqs, type $t, unres ${cs.unsolved}")
      else if (!cs.isSolved)
        Right(s"Unresolved constraints ${cs.unsolved}, type $t")
      else
        Left(t)
    }
  }

  def addFieldReq(creqs: CR, t: Type, f: Symbol, U: Type): (Seq[Constraint], CR) = {
    //if (t == CName('Object))
    //  return (Seq()) TODO shouldn't class reqs be constraints?
    creqs.cr.get(t) match {
      case None =>
        val res: CR = CR(creqs.cr + (t -> ClassReq(None, None, Map(f -> U), Map(), Map())))
        (Seq(), res)
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        fields.get(f) match {
          case None =>
            (Seq(), creqs + (t -> ClassReq(sup, ctor, fields + (f -> U), methods, cmethods)))
          case Some(t2) =>
            (Seq(Equal(t2, U)), creqs)
        }
    }
  }

  def addFieldDec(creqs: CReqs, t: Type, f: Symbol, U: Type): (Seq[Constraint], CReqs) = {
    //if (t == CName('Object))
    //  return (Seq()) TODO shouldn't class reqs be constraints?
    creqs.get(t) match {
      case None =>
        (Seq(), creqs)
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        fields.get(f) match {
          case None =>
            (Seq(), creqs)
          case Some(t2) =>
            val fnew = fields - f
            (Seq(Equal(t2, U)), creqs - t+ (t -> ClassReq(sup, ctor, fnew, methods, cmethods)))
        }
    }
  }

  def addMethodReq(creqs: CReqs, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CReqs) = {
    creqs.get(t) match {
      case None =>
        val res: CReqs = creqs + (t -> ClassReq(None, None, Map(), Map(m ->(ret, args)), Map()))
        (Seq(), res)
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        methods.get(m) match {
          case None =>
            (Seq(), creqs + (t -> ClassReq(sup, ctor, fields, methods + (m ->(ret, args)), cmethods)))

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

  def addCMethodReq(creqs: CReqs, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CReqs) = {
    var res = creqs
    if (t == CName('Object)) {
      (Seq(), creqs)
    }
    else {
      creqs.get(t) match {
        case None =>
          val res: CReqs = creqs + (t -> ClassReq(None, None, Map(), Map(), Map(m ->(ret, args))))
          (Seq(), res)
        case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
          cmethods.get(m) match {
            case None =>
              (Seq(), creqs + (t -> ClassReq(sup, ctor, fields, methods, cmethods + (m ->(ret, args)))))

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

  def addMethodDec(creqs: CReqs, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CReqs) = {
    creqs.get(t) match {
      case None =>
        (Seq(), creqs)
      case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
        methods.get(m) match {
          case None =>
            (Seq(), creqs)
          case Some((ret2, args2)) =>
            if (args.length == args2.length) {
              val cons = Equal(ret2, ret) +: (args2 zip args).map(p => Equal(p._1, p._2))
              val mnew = methods - m
              (cons, creqs - t + (t -> ClassReq(sup, ctor, fields, mnew, cmethods)))
            }
            else
              (Seq(Equal(ret, ret2), Never(AllEqual(args, args2))), creqs)
        }
    }
  }


  def addCtorReq(creqs: CReqs, t: Type, params: List[Type]): (Seq[Constraint], CReqs) = {
    var res = creqs
    if (t == CName('Object)) {
      (Seq(), creqs)
    }
    else {
      creqs.get(t) match {
        case None =>
          res = creqs + (t -> ClassReq(None, Some(params), Map(), Map(), Map()))
          (Seq(), res)
        case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
          ctor match {
            case None =>
              (Seq(), res + (t -> ClassReq(sup, Some(params), fields, methods, cmethods)))
            case Some(params2) =>
              if (params.length == params2.length) {
                val cons = (params zip params2).map(p => Equal(p._1, p._2))
                (cons, res)
              }
              else
                (Seq(Never(AllEqual(params, params2))), res)
          }
      }
    }
  }

  def addSupertypeReq(creqs: CReqs, t: Type, tsuper: Type): (Seq[Constraint], CReqs) = {
    var res = creqs
    if (tsuper == CName('Object)) {
      (Seq(), creqs)
    }
    else {
      res = res + (tsuper -> ClassReq(None, None, Map(), Map(), Map()))
      creqs.get(t) match {
        case None =>
          res = res + (t -> ClassReq(Some(tsuper), None, Map(), Map(), Map()))
          (Seq(), res)
        case Some(ClassReq(sup, ctor, fields, methods, cmethods)) =>
          sup match {
            case None =>
              (Seq(), res + (t -> ClassReq(Some(tsuper), ctor, fields, methods, cmethods)))
            case Some(t2) =>
              (Seq(Equal(tsuper, t2)),res)
          }
      }
    }
  }

  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {

    case Num =>
      (CName('TNum), Map(), Map(),Seq())
    case Str =>
      (CName('TString), Map(), Map(), Seq())

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
      (X, Map(x -> X), Map(), Seq()) // Map(X -> cld), needed at some examples

    case Fields =>
      val f = e.lits(0).asInstanceOf[Symbol] //symbol
      val (t, reqs, creqs, _) = e.kids(0).typ //subsol
      val U = freshCName()
      val (cons, mcreqs) = addFieldReq(creqs, t, f, U)

      (U, reqs, mcreqs, cons) //subsol

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (te, reqs0, creqs0,  _) = e.kids(0).typ
      val U = freshCName()
      var cons = Seq[Constraint]()
      var reqss: Seq[Reqs] = Seq(reqs0)
      var creqss: Seq[CReqs] = Seq(creqs0)
      var param = List[Type]()
      for (i <- 1 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids(i).typ
        val Ui = freshCName()
       reqss = reqss :+ subreqs
       cons = Subtype(ti, Ui) +: cons //or should be subtype
       creqss = creqss :+ subcreqs
       param = param :+ Ui
      }

      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)
      val (mcCons, mcreqs) = addMethodReq(creqs, te, m, param, U)

      (U, mreqs, mcreqs, mcons ++ cCons ++ mcCons ++ cons)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      val U = freshCName()
      var reqss = Seq[Reqs]()
      var creqss = Seq[CReqs]()
      var cons = Seq[Constraint]()
      var ctor: List[Type] = Nil
      for (i <- 0 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids.seq(i).typ
        val Ui = freshCName()
        ctor = Ui :: ctor
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
       cons = Subtype(ti, Ui) +: cons //or should be subtype
      }
      ctor = ctor.reverse
      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)

      val (mcCons, mcreqs) = addCtorReq(creqs, c, ctor)

      (c, mreqs, mcreqs, mcons ++ cCons ++ cons++ mcCons)

    case DCast =>
      val (t, reqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

     // val bcreqs : BCReqs = (None, ClassDecl(None, None, Map(), Map()), false)
      (c, reqs, creqs, Seq((NotEqual(t, c))))

    case UCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs,_) = e.kids(0).typ

     // val bcreqs : BCReqs = (None, ClassDecl(None, None, Map(), Map()), false)
      (c, reqs, creqs, Seq(Subtype(t, c)))

    case SCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs, _) = e.kids(0).typ

    //  val bcreqs : BCReqs = (None, ClassDecl(None, None, Map(), Map()), false)
      (t, reqs, creqs, Seq())

    case MethodDec =>
      val (e0, reqs, creqs, _) = e.kids(0).typ
      val retT = e.lits(0).asInstanceOf[CName]
      val m = e.lits(1).asInstanceOf[Symbol]
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]
      var restReqs = reqs
      var cons = Seq[Constraint]()
      var cCreqs = creqs
      cons = Subtype(e0, retT) +: cons
      val Uc = freshCName()
      val Ud = freshCName()
      cons = Extend(Uc, Ud) +: cons
      for ((x, xC) <- params) {
        reqs.get(x) match {
          case None => restReqs = restReqs
          case Some(typ) =>
            restReqs = restReqs - x
            cons = Equal(typ, xC) +: cons
        }
      }
     val (ccons, cres)= addCMethodReq(cCreqs, Ud, m, params.toMap.valuesIterator.toList, retT)

     restReqs.get('this) match {
        case None => restReqs = restReqs
        case Some(typ) =>
          cons = Equal(typ, Uc) +: cons // Subtype(Uc, typ)
          restReqs = restReqs - 'this
       }
      cons= cons ++ ccons
      (MethodOK(Uc), restReqs, cres, cons )
    case ClassDec =>
      val c = e.lits(0).asInstanceOf[Type]
      val sup = e.lits(1).asInstanceOf[Type]
      val Ctor(params, superCall, fieldDefs) = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()
      var restReqs = Seq[Reqs]()
      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, _) = e.kids(i).typ
        restReqs = restReqs :+ req
        val retT = e.kids(i).lits(0).asInstanceOf[CName]
        val m = e.kids(i).lits(1).asInstanceOf[Symbol]
        val params = e.kids(i).lits(2).asInstanceOf[Seq[(Symbol, Type)]].unzip._2.toList
        val (mcons, cr) = addMethodDec(creq, c, m, params, retT)
        restCreq = cr +: restCreq
        cons = cons ++ mcons :+ Equal(t.asInstanceOf[MethodOK].in, c)
      }
      val (conss, cr) = mergeCReqMaps(restCreq)
      val (rcons, req) = mergeReqMaps(restReqs)
      cons = cons ++ conss :+ Extend(c, sup)
      val (mcons3, cr3) = addSupertypeReq(cr, c, sup) //cr2
      val (mcons4, cres) = fields.foldLeft((mcons3, cr3)) {case ((mcns, creqs), (field, tpe)) =>
        val (mconsfold, creqsfold) = addFieldDec(creqs, c, field, tpe)
      (mcns ++ mconsfold, creqsfold)
     }
      val lst = Ctor(params, superCall, fieldDefs).params.valuesIterator.toList
      val ( consC,cresC) = addCtorReq(cres, c, lst)
      val ( consD, cresD) = addCtorReq(cresC, sup, lst.dropRight(Ctor(params, superCall, fieldDefs).fieldDefs.size))

      cons = cons ++ mcons4 ++ consC ++ consD

      val  subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
      val cs = subcs addNewConstraints cons
      val cresCon = removeCondReq(cresD)
      val (cReqs, cconss) =  remove(cresCon, List(c), cs)
      cons = cons ++ cconss
      (c, req, cReqs, cons)

    case ProgramM =>
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()
      var cls = List[Type]()
      for (i <- 0 until e.kids.seq.size) {
        val (c, _, cres,  _) = e.kids(i).withType[StepResult].typ
        restCreq = cres +: restCreq
        cls = cls :+ c
      }

      val  subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
      var (mCcons, mcreqs) = mergeCReqMaps(restCreq)
      cons = cons ++ mCcons
      val cs = subcs addNewConstraints cons
      val cres = removeCondReq(mcreqs)
      val (cr, ctcons)=  remove(cres, cls, cs)

      (CName('Object), Map(), cr, cons ++ ctcons)
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
         mcons = mcons :+ Equal(r1, r2)
      }
    (mcons, mreqs)
  }

  private def remove( creq : CReqs, cls : List[Type], cs :CS) : (CReqs,Seq[Constraint]) = {
    var cr = creq
    var cons = Seq[Constraint]()
    for (i <- 0 until cls.length) {
      creq.get(cls(i)) match {
        case None => creq
        case Some(cld) =>
            var stype = cld.extendc
            var fields = cld.fields
          val ctor = cld.ctorParams // TODO check again the usage of CTor when we remove
            var methods = cld.methods
            for ((d, cldD) <- creq) {
              if (cs.isSubtype(cls(i), d)) {
                for ((f, typ) <- fields) {
                  cldD.fields.get(f) match {
                    case None => fields = fields
                    case Some(typ2) =>
                      fields = fields - f
                      cons = cons :+ Equal(typ2, typ)
                  }
                }
                for ((m, rt) <- methods) {
                  cldD.methods.get(m) match {
                    case None => methods = methods
                    case Some(rt2) =>
                      cons = cons :+ Equal(rt2._1, rt._1)
                      for (i <- 0 until rt._2.length)
                        cons = cons :+ Equal(rt2._2(i), rt._2(i))
                      methods = methods - m
                  }
                }
              }
            }
          if (fields.isEmpty && methods.isEmpty && cld.cmethods.isEmpty)
          cr = cr - cls(i)
          }
     for ((c, cld) <- cr){
        if (c.isInstanceOf[UCName]) {
          cr = cr - c
        }
        else cr
      }
    }
  (cr,cons)
  }

 def removeCondReq(creq : CReqs) : CReqs = {
   var cres = creq
   var cldm = Map[Symbol, (Type, List[Type])]()
   var cldCm = Map[Symbol, (Type, List[Type])]()
   for ((c, cld) <- creq) {
     if (c == CName('Object)) cres = cres - c
     else {
       cldCm = cld.cmethods
       cldm = cld.methods
       for ((m, dec) <- cld.cmethods) {
         cld.methods.get(m) match {
           case None => cldCm = cldCm - m
           case Some(dec2) =>
             cldm += (m -> (dec._1, dec._2))
           //  cons = cons +: Equal(dec._1, dec2._1) ++ AllEqual(dec._2, dec2._2) TODO Not sure if constrains should be added or not. OR just replace the signature
             cldCm = cldCm - m
         }
       }
       cres += (c -> ClassReq(cld.extendc,cld.ctorParams, cld.fields, cldm, cldCm))

     }
   }
   cres
 }

  private val cinit: (Seq[Constraint], CReqs) = (Seq(), Map())

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
       case (Some(t1), Some(t2)) => ctor
         if (t1.size == t2.size) mcons = mcons :+ AllEqual(t1, t2)
          else mcons = mcons :+ Never(AllEqual(t1, t2))
      }
    for ((f, typ) <- newF){
      wasF.get(f) match {
        case None => rF += (f -> typ)
        case Some(typ2) =>
          mcons = mcons :+ Equal(typ2, typ)
      }
    }
    for ((m, mbody) <- cld2.methods) {
      wasM.get(m) match {
        case None => cldm += (m -> mbody) // mdoby = return type + list of parameters
        case Some(mbody2) =>
          mcons = mcons :+ Equal(mbody2._1, mbody._1)
          if (mbody._2.length == mbody2._2.length)
            mcons = mcons :+ AllEqual(mbody._2, mbody2._2)
          else mcons = mcons :+ Never(AllEqual(mbody._2, mbody2._2))
      }
    }
    for ((m, mbody) <- cld2.cmethods) {
      cld1.cmethods.get(m) match {
        case None => cldmc += (m -> mbody) // mdoby = return type + list of parameters
        case Some(mbody2) =>
          mcons = mcons :+ Equal(mbody2._1, mbody._1)
          if (mbody._2.length == mbody2._2.length)
            mcons = mcons :+ AllEqual(mbody._2, mbody2._2)
          else mcons = mcons :+ Never(AllEqual(mbody._2, mbody2._2))
      }
    }
    var styp : Option[Type] = cld1.extendc
    (cld1.extendc, cld2.extendc) match {
      case (None, None) => styp
      case (None, Some(_)) => styp = cld2.extendc
      case (Some(_), None) => styp
      case (Some(t1), Some(t2)) => styp
        mcons = mcons :+ Equal(t1, t2)
    }
    (mcons, ClassReq(styp, ctor, rF, cldm, cldmc))
  }


  def mergeCReqMaps(creq: CReqs, creqs: CReqs*): (Seq[Constraint], CReqs) = mergeCReqMaps(creq +: creqs)

  def mergeCReqMaps(creqs: Seq[CReqs]): (Seq[Constraint], CReqs) =
    Util.timed(localState -> Statistics.mergeCReqsTime) {
      creqs.foldLeft[(Seq[Constraint], CReqs)](cinit)(_mergeCReqMaps)
    }

  private def _mergeCReqMaps(was: (Seq[Constraint], CReqs), newCReqs: CReqs) = {
    val wasCReqs = was._2
    var mcons = was._1
    var mcreqs = wasCReqs
    for ((t, cld2) <- newCReqs)
      wasCReqs.get(t) match {
        case None => mcreqs += (t -> cld2)
        case Some(cld1) => mcreqs = mcreqs + (t -> mergeCCld(cld1, cld2)._2)
          mcons = mergeCCld(cld1, cld2)._1 ++ mcons
      }
    (mcons, mcreqs)
  }


 /* def mergeBoolReq(creqs: CReqs, breqs : Seq[BCReqs]) : Seq[Constraint] = {
    var cons = Seq[Constraint]()
    for ((d, cld, bol)<-  breqs) {
      (d, cld, bol) match {
        case (None, m1, false) => cons
        case (Some(t), m, false) => cons
        case (Some(t), (m, re, param), true) =>
      creqs.get(t) match {
        case None => cons
        case Some(cld2) =>
         for((m1, body) <- cld2.methods ) {
           if (m1 == m) {
               re match {
                 case None => cons
                 case Some(re2) =>
                   cons = cons :+ Equal(body._1, re2) :+ AllEqual(param, body._2)
               }}
           else cons
         }}}}
    cons
  }*/

}


case class BUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new BUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}
