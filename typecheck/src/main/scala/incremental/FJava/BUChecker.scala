package incremental.fjava

import constraints.fjava.CSubst.CSubst
import constraints.{CVar, Statistics}
import constraints.fjava._
import constraints.fjava.impl
import incremental.{NodeKind, Node_, Util}
import incremental.Node._

import scala.collection.immutable.{Map, ListMap}

/**
 * Created by lirakuci on 3/2/15.
 */


case class FieldName(x: Symbol)
case class Param(x: Symbol)


abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String

  type Methods = Map[Symbol, (Type, List[Type])]

  type Fields = Map[Symbol, Type]

  case class ClassDecl(superType: Option[Type], ctorParams: Option[List[Type]], fields: Fields, methods: Methods) {
    def subst(s: CSubst) = ClassDecl(superType.map.(_.subst(s)), ctorParams.map(_.map(_.subst(s))), fields.mapValues(_.subst(s)), methods.mapValues {case (ret, args) => (ret.subst(s), args.map(_.subst(s)))})
  }//(supertype, Fields, Methods)


  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Type, ClassDecl]

  type StepResult = (Type, Reqs, CReqs, Seq[Constraint])
  type Result = (Type, Reqs, CReqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val (t, reqs, creqs, cons) = typecheckStep(e)
        val  subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
        val cs = subcs addNewConstraints cons
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        var creqs2: CReqs = Map()
        for ((t, decl) <- creqs) {
          val c = t.subst(cs.substitution)
          val decl2 = decl.subst(cs.substitution)
          creqs2 +=  (c -> decl2)
        }
        e.typ = (cs applyPartialSolution t, reqs2, creqs2, cs.propagate)
        true
      }

      val (t_, reqs, creqs, cs_) = root.typ
      val cs = cs_.tryFinalize
      val t = t_.subst(cs.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved variable requirements $reqs, type $t, unres ${cs.unsolved}")
      else if (!creqs.isEmpty)
        Right(s"Unresolved type-variable requirements $creqs, type $t, unres ${cs.unsolved}")
      else if (!cs.isSolved)
        Right(s"Unresolved constraints ${cs.unsolved}, type $t")
      else
        Left(t)
    }
  }

  def addFieldReq(creqs: CReqs, t: Type, f: Symbol, U: Type): (Seq[Constraint], CReqs) = {
    //if (t == CName('Object))
    //  return (Seq()) TODO shouldn't class reqs be constraints?
    creqs.get(t) match {
      case None =>
        val res: CReqs = creqs + (t -> ClassDecl(None, None, Map(f -> U), Map()))
        (Seq(), res)
      case Some(ClassDecl(sup, ctor, fields, methods)) =>
        fields.get(f) match {
          case None =>
            (Seq(), creqs + (t -> ClassDecl(sup, ctor, fields + (f -> U), methods)))

          case Some(t2) =>
            (Seq(Equal(U, t2)), creqs)
        }
    }
  }

  def addMethodReq(creqs: CReqs, t: Type, m: Symbol, args: List[Type], ret: Type): (Seq[Constraint], CReqs) = {
    creqs.get(t) match {
      case None =>
        val res: CReqs = creqs + (t -> ClassDecl(None, None, Map(), Map(m -> (ret, args))))
        (Seq(), res)
      case Some(ClassDecl(sup, ctor, fields, methods)) =>
        methods.get(m) match {
          case None =>
            (Seq(), creqs + (t -> ClassDecl(sup, ctor, fields, methods + (m -> (ret, args)))))

          case Some((ret2, args2)) =>
            if (args.length == args2.length) {
              val cons = Equal(ret, ret2) +: (args zip args2).map(p => Equal(p._1, p._2))
              (cons, creqs)
            }
            else
              (Seq(Equal(ret, ret2), Never(AllEqual(args, args2))), creqs)
        }
    }
  }

  def addCtorReq(creqs: CReqs, t: Type, params: List[Type]): (Seq[Constraint], CReqs) = {
    creqs.get(t) match {
      case None =>
        val res: CReqs = creqs + (t -> ClassDecl(None, Some(params), Map(), Map()))
        (Seq(), res)
      case Some(ClassDecl(sup, ctor, fields, methods)) =>
        ctor match {
          case None =>
            (Seq(), creqs + (t -> ClassDecl(sup, Some(params), fields, methods)))

          case Some(params2) =>
            if (params.length == params2.length) {
              val cons = (params zip params2).map(p => Equal(p._1, p._2))
              (cons, creqs)
            }
            else
              (Seq(Never(AllEqual(params, params2))), creqs)
        }
    }
  }

  def addSupertypeReq(creqs: CReqs, t: Type, tsuper: Type): (Seq[Constraint], CReqs) = {
    creqs.get(t) match {
      case None =>
        val res: CReqs = creqs + (t -> ClassDecl(Some(tsuper), None, Map(), Map()))
        (Seq(), res)
      case Some(ClassDecl(sup, ctor, fields, methods)) =>
        sup match {
          case None =>
            (Seq(), creqs + (t -> ClassDecl(Some(tsuper), ctor, fields, methods)))

          case Some(t2) =>
            (Seq(Equal(tsuper, t2)), creqs)
        }
    }
  }



  def typecheckStep(e: Node_[Result]): StepResult = e.kind match {

    case Num => (CName('TNum), Map(), Map(), Seq())
    case Str => (CName('TString), Map(), Map(), Seq())
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
      val (te, reqs0, creqs0, _) = e.kids(0).typ
      val U = freshCName()
      var cons = Seq[Constraint]()
      var reqss: Seq[Reqs] = Seq(reqs0)
      var creqss: Seq[CReqs] = Seq(creqs0)
      var param = List[Type]()
      for (i <- 1 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids.seq(i).typ
        val Ui = freshCName()
       reqss = reqss :+ subreqs
       cons = Subtype(ti, Ui) +: cons
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
        cons = Subtype(ti, Ui) +: cons
      }
      ctor = ctor.reverse
      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)

      val (mcCons, mcreqs) = addCtorReq(creqs, c, ctor)
      (c, mreqs, mcreqs, mcons ++ cCons ++ mcCons ++cons)

    case DCast =>
      val (t, reqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, creqs, Seq((NotEqual(t, c))))

    case UCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs, _) = e.kids(0).typ
      (c, reqs, creqs, Seq(Subtype(t, c)))

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
            cons = Subtype(xC, typ) +: cons
        }
      }
      restReqs.get('this) match {
         case None => restReqs = restReqs
        case Some(typ) =>
           cons = Equal(Uc, typ) +: cons // Subtype(Uc, typ)
           restReqs = restReqs - 'this
       }
      (MethodOK(Uc), Map(), cCreqs, cons )
    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val Ctor(params, superCall, fieldDefs) = if (e.lits.size > 2) e.lits(2).asInstanceOf[Ctor] else Ctor(ListMap(), List(), ListMap())
      val fields = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()

      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, _) = e.kids.seq(i).typ
        val retT = e.kids.seq(i).lits(0).asInstanceOf[CName]
        val m = e.kids.seq(i).lits(1).asInstanceOf[Symbol]
        val params = e.kids.seq(i).lits(2).asInstanceOf[Seq[(Symbol, Type)]].unzip._2.toList
        val (mcons, cr) = addMethodReq(creq, c, m, params, retT)
        restCreq = cr +: restCreq
        cons = cons ++ mcons :+ Equal(c, t.asInstanceOf[MethodOK].in)
      }
      val (conss, cr) = mergeCReqMaps(restCreq)

      val superCtorSig: List[Type] = params.toList.take(superCall.length).map(_._2)
      val (mcons2, cr2) = addCtorReq(cr, sup, superCtorSig)
      val (mcons3, cr3) = addSupertypeReq(cr2, c, sup)
      val (mcons4, cres) = fields.foldLeft((mcons3, cr3)) {case ((mcns, creqs), (field, tpe)) =>
        val (mconsfold, creqsfold) = addFieldReq(creqs, c, field, tpe)
        (mcns ++ mconsfold, creqsfold)
      }
      cons = cons ++ conss ++ mcons2 ++ mcons4
      cons = Extend(c, sup) +: cons
      (ClassOK, Map(), cres, cons) //tc

    case ProgramM =>
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()
      for (i <- 0 until e.kids.seq.size) {
        val (ClassOK, _, cres, econs) = e.kids(i).withType[StepResult].typ
        restCreq = cres +: restCreq
        cons = cons ++ econs
      }

      val  subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)

      var (mCcons, mcreqs) = mergeCReqMaps(restCreq)
      cons = cons ++ mCcons //++ ctons

    //  val cs = subcs addNewConstraints cons
     val (ct, ctcons) =  remove( mcreqs, subcs)

      (CName('Object),Map(), mcreqs,  cons ++ ctcons)
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

  private def remove( creq : CReqs, cs :CS) : (CReqs, Seq[Constraint]) = {
    var cr = creq
    var cons = Seq[Constraint]()
    var ct = CT
    for ((c, cld) <- creq) {
      var stype = cld._1
      var fields = cld._2
      var methods = cld._3
     for ((d, cldD) <- CT){
        if (cs.isSubtype(c,d)){
          for ((f, typ) <- fields){
            cldD._2.get(f) match {
              case None => fields = fields
              case Some(typ2) =>
                fields = fields - f
                cons = cons :+ Equal(typ, typ2)
            }
          }
          for ((m, rt) <- methods){
            cldD._3.get(m) match {
              case None => methods = methods
              case Some(rt2) =>
                cons = cons :+ Equal(rt._1, rt2._1)
                for (i <- 0 until rt._2.length)
                  cons = cons :+ Equal(rt._2(i), rt2._2(i))
                methods = methods - m
            }
          }
        }
      }
    cr = cr - c
    }
    (cr, cons)
  }

  private val cinit: (Seq[Constraint], CReqs) = (Seq(), Map())

  def mergeCCld(cld1: ClassDecl, cld2: ClassDecl): (Seq[Constraint], ClassDecl) = {
    val newF = cld2._2
    val wasF = cld1._2
    var rF = wasF
    val wasM = cld1._3
    var mcons = Seq[Constraint]()
    var cldm = wasM
   // var p = List[Type]()
    for ((f, typ) <- newF){
      wasF.get(f) match {
        case None => rF += f -> typ
        case Some(typ2) =>
          rF = rF
          mcons = mcons :+ Equal(typ, typ2)
      }
    }
    for ((m, mbody) <- cld2._3) {
      wasM.get(m) match {
        case None => cldm += (m -> mbody) // mdoby = return type + list of parameters
        case Some(mbody2) =>
          mcons = mcons :+ Equal(mbody._1, mbody2._1)
          if (mbody._2.length == mbody2._2.length) {
            for (i <- 0 until mbody._2.length)
              mcons = mcons :+ Equal(mbody._2(i), mbody2._2(i))
          }
          cldm = cldm
      }
    }
    if (cld1._1.length > 0 && cld2._1.length > 0)
    mcons = Equal(cld1._1.head, cld2._1.head) +: mcons
    val cld: ClassDecl = (cld1._1, rF, cldm)
    (mcons, cld)
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
        case Some(cld1) => mcreqs = mcreqs - t + (t -> mergeCCld(cld1, cld2)._2)
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
