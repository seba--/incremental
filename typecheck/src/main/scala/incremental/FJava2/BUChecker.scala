package incremental.FJava2

import constraints.{CTermBase, Statistics}
import constraints.fjava.CSubst.CSubst
import constraints.fjava._
import incremental.Node._
import incremental.{Node_, Util}

import scala.collection.immutable.ListMap
/**
 * Created by lirakuci on 3/2/15.
 */

import Condition.trueCond

trait CReq[T] {
  val self: T
  val cls: Type
  val cond: Condition
  def canMerge(other: CReq[T]): Boolean
  def assert(other: CReq[T]): Constraint
  def subst(s: CSubst): Option[T]

  def withCond(cond: Condition): T
  def alsoNot(n: Type): Option[T] = if (cls == n || cond.same.contains(n)) None else Some(withCond(Condition(cond.not :+ n, cond.same)))
  def alsoSame(n: Type): Option[T] = if (cls.isGround && n.isGround && cls != n || cond.not.contains(n)) None else Some(withCond(Condition(cond.not, cond.same :+ n)))
  def alsoCond(other: Condition): Option[T] = if (other.not.contains(cls) || other.not.exists(cond.same.contains(_)) || cond.not.exists(other.same.contains(_))) None else Some(withCond(Condition(cond.not ++ other.not, cond.same ++ other.same)))
}
case class ExtCReq(cls: Type, ext: Type, cond: Condition = trueCond) extends CReq[ExtCReq] {
  val self = this
  def canMerge(other: CReq[ExtCReq]) = true
  def assert(other: CReq[ExtCReq]) = Conditional(cls, other.self.cls, Equal(ext, other.self.ext))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (ExtCReq(cls_, ext.subst(s), _))
  }
  def lift = ClassReqs(ext = Seq(this))
  def withCond(c: Condition) = copy(cond = c)
}
case class CtorCReq(cls: Type, args: Seq[Type], cond: Condition = trueCond) extends CReq[CtorCReq] {
  val self = this
  def canMerge(other: CReq[CtorCReq]) = true
  def assert(other: CReq[CtorCReq]) = Conditional(cls, other.self.cls, AllEqual(args, other.self.args))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (CtorCReq(cls_, args.map(_.subst(s)), _))
  }
  def lift = ClassReqs(ctorParams = Seq(this))
  def withCond(c: Condition) = copy(cond = c)
}
case class FieldCReq(cls: Type, field: Symbol, typ: Type, cond: Condition = trueCond) extends CReq[FieldCReq] {
  val self = this
  def canMerge(other: CReq[FieldCReq]): Boolean = field == other.self.field
  def assert(other: CReq[FieldCReq]) = Conditional(cls, other.self.cls, Equal(typ, other.self.typ))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (FieldCReq(cls_, field, typ.subst(s), _))
  }
  def lift = ClassReqs(fields = Seq(this))
  def withCond(c: Condition) = copy(cond = c)
}
case class MethodCReq(cls: Type, name: Symbol, params: Seq[Type], ret: Type, cond: Condition = trueCond) extends CReq[MethodCReq] {
  val self = this
  def canMerge(other: CReq[MethodCReq]): Boolean = name == other.self.name
  def assert(other: CReq[MethodCReq]) = Conditional(cls, other.self.cls, AllEqual(params :+ ret, other.self.params :+ other.self.ret))
  def subst(s: CSubst) = {
    val cls_ = cls.subst(s)
    cond.subst(cls_, s) map (MethodCReq(cls_, name, params.map(_.subst(s)), ret.subst(s), _))
  }
  def liftOpt = ClassReqs(optMethods = Seq(this))
  def lift = ClassReqs(methods = Seq(this))
  def withCond(c: Condition) = copy(cond = c)
}

object Condition {
  def trueCond = Condition(Seq(), Seq())
}
case class Condition(not: Seq[Type], same: Seq[Type]){
  def subst(cls: Type, s: CSubst): Option[Condition] = {
    val newnot = not flatMap { n =>
      val n2 = n.subst(s)
      if (cls == n2)
        return None
      else if (cls.isGround && n2.isGround) // && cls != n2 (implicit)
        None
      else
        Some(n2)
    }
    val newsame = same flatMap { n =>
      val n2 = n.subst(s)
      if (cls == n2)
        None
      else if (cls.isGround && n2.isGround) // && cls != n2 (implicit)
        return None
      else
        Some(n2)
    }
    Some(Condition(newnot, newsame))
  }
}

case class ClassReqs (
    currentClass: Option[Type] = None,
    ext: Seq[ExtCReq] = Seq(),
    ctorParams: Seq[CtorCReq] = Seq(),
    fields: Seq[FieldCReq] = Seq(),
    methods: Seq[MethodCReq] = Seq(),
    optMethods: Seq[MethodCReq] = Seq()) {

  override def toString =
    s"ClassReqs(current=$currentClass, ext=$ext, ctorParams=$ctorParams, fields=$fields, methods=$methods, optMethods=$optMethods)"

  def subst(s: CSubst): ClassReqs = ClassReqs(
    currentClass.map(_.subst(s)),
    subst(ext, s),
    subst(ctorParams, s),
    subst(fields, s),
    subst(methods, s),
    subst(optMethods, s))

  private def subst[T <: CReq[T]](crs: Seq[T], s: CSubst): Seq[T] = crs.flatMap (_.subst(s))

  def isEmpty = currentClass.isEmpty && ext.isEmpty && ctorParams.isEmpty && fields.isEmpty && methods.isEmpty

  def merge(crs: ClassReqs): (ClassReqs, Seq[Constraint]) = {
    val (currentX, cons0) = (currentClass.orElse(crs.currentClass), for (t1 <- currentClass; t2 <- crs.currentClass) yield Equal(t1, t2))
    val (extX, cons1) = merge(ext, crs.ext)
    val (ctorX, cons2) = merge(ctorParams, crs.ctorParams)
    val (fieldsX, cons3) = merge(fields, crs.fields)
    val (methodsX, cons4) = merge(methods, crs.methods)
    val (optMethodsX, cons5) = merge(optMethods, crs.optMethods)
    val cons = cons0.toSeq ++ cons1 ++ cons2 ++ cons3 ++ cons4 ++ cons5
    (ClassReqs(currentX, extX, ctorX, fieldsX, methodsX, optMethodsX), cons)
  }

  private def merge[T <: CReq[T]](crs1: Seq[T], crs2: Seq[T]): (Seq[T], Seq[Constraint]) = {
    if (crs1.isEmpty)
      return (crs2, Seq())
    if (crs2.isEmpty)
      return (crs1, Seq())

    var cons = Seq[Constraint]()
    val cr = crs1.flatMap ( cr1 =>
      crs2.flatMap ( cr2 =>
        if (cr1.canMerge(cr2)) {
          val reqDiff1 = cr1.alsoNot(cr2.cls)
          val reqDiff2 = cr2.alsoNot(cr1.cls)
          val reqSame = cr1.alsoCond(cr2.cond).flatMap(_.alsoSame(cr2.cls))
          cons = cons :+ cr1.assert(cr2)
          Seq(reqDiff1, reqDiff2, reqSame).flatten
        }
        else
          Seq(cr1, cr2)
      )
    )
    (cr, cons)
  }

  def satisfyCurrentClass(cls: CName): (ClassReqs, Seq[Constraint]) = {
    currentClass match {
      case None => (this, Seq())
      case Some(t) => (copy(currentClass = None), Seq(Equal(cls, t)))
    }
  }
  def satisfyExtends(ext: ExtCReq): (ClassReqs, Seq[Constraint]) = satisfyCReq[ExtCReq](ext, this.ext, x=>copy(ext=x))
  def satisfyCtor(ctor: CtorCReq): (ClassReqs, Seq[Constraint]) = satisfyCReq[CtorCReq](ctor, ctorParams, x=>copy(ctorParams=x))
  def satisfyField(field: FieldCReq): (ClassReqs, Seq[Constraint]) = satisfyCReq[FieldCReq](field, fields, x=>copy(fields=x))

  // TODO optMethods
  def satisfyMethod(method: MethodCReq): (ClassReqs, Seq[Constraint]) = satisfyCReq[MethodCReq](method, methods, x=>copy(methods=x))

  private def satisfyCReq[T <: CReq[T]](creq1: T, crs: Seq[T], make: Seq[T] => ClassReqs): (ClassReqs, Seq[Constraint]) = {
    var cons = Seq[Constraint]()
    val newcrs = crs flatMap ( creq2 =>
      if (creq1.canMerge(creq2)) {
        cons = cons :+ creq1.assert(creq2)
        creq2.alsoNot(creq1.cls)
      }
      else
        Some(creq2)
    )
    (make(newcrs), cons)
  }

  def many[T <: CReq[T]](f: ClassReqs => T => (ClassReqs, Seq[Constraint]), reqs: Iterable[T]) = {
    var cons = Seq[Constraint]()
    var creqs = this
    for (req <- reqs) {
      val (newcreqs, newcons) = f(creqs)(req)
      creqs = newcreqs
      cons = cons ++ newcons
    }
    (creqs, cons)
  }
}




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
      val fieldInitCons = currentClassCons :+ AllEqual(fields.values.toList, ctor.fields.values.toList)
      // constructor provides correct arguments to super constructor
      val (creqs2, supCons) = creqs.merge(CtorCReq(sup, ctor.superParams.values.toList).lift)

      (c, reqs, creqs2, mccons ++ mrcons ++ currentClassCons ++ supCons ++ fieldInitCons)

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
      for (cls <- e.kids.seq) {
        val cname = cls.lits(0).asInstanceOf[CName]
        val sup = cls.lits(1).asInstanceOf[CName]
        val ctor = cls.lits(2).asInstanceOf[Ctor]
        val fields = cls.lits(3).asInstanceOf[Seq[(Symbol, Type)]].toMap
        val methods = cls.kids.seq

        val (creqs1, cons1) = restCReqs.satisfyCtor(CtorCReq(cname, ctor.allArgTypes))
        val (creqs2, cons2) = creqs1.satisfyExtends(ExtCReq(cname, sup))
        val (creqs3, cons3) = creqs2.many(_.satisfyField, fields map (f => FieldCReq(cname, f._1, f._2)))
        val (creqs4, cons4) = creqs3.many(_.satisfyMethod,
          methods map (m => MethodCReq(
            cname,
            m.lits(1).asInstanceOf[Symbol],
            m.lits(2).asInstanceOf[Seq[(Symbol, Type)]].map(_._2),
            m.lits(0).asInstanceOf[Type])))

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
