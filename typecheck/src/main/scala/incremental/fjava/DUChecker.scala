package incremental.fjava

/**
 * Created by lirakuci on 10/26/16.
 */
import constraints.Statistics
import constraints.fjava.CSubst.CSubst
import constraints.fjava._
import incremental.Node._
import incremental.{pcf, Util, Node_}
import incremental.fjava.ClassReqs
import incremental.fjava.Condition
import incremental.fjava.CtorCReq
import incremental.fjava.ExtCReq
import incremental.fjava.FieldCReq
import incremental.fjava.MethodCReq
/**
 * Created by lirakuci on 3/2/15.
 */

import incremental.fjava.Condition.trueCond

trait CTcls[T <: CTcls[T]] {
  def self: T
}

case class ExtCT(cls: Type, ext: Type) extends CTcls[ExtCT] {
  def self = this
 // def subst(s: CSubst) =  ExtCT(cls.subst(s), ext.subst(s))
  }
case class CtorCT(cls: Type, args: Seq[Type]) extends CTcls[CtorCT] {
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
                ctorParams: Set[CtorCT] = Set(),
                fields: Set[FieldCT] = Set(),
                methods: Set[MethodCT] = Set())


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

  def field(f : Symbol, cls : Type, ct: CT): FieldCT = {
    val posFTyp = ct.fields.find(ftyp => (ftyp.field == f) && (ftyp.cls == cls) )
    posFTyp match {
      case None => ct.ext.toMap[Type,Type].get(cls)   match {
        case None => ???
        case Some(superCls) =>  field(f, superCls, ct)
      }
      case Some(fTyp) => FieldCT(fTyp.cls, fTyp.field , fTyp.typ)
    }
  }

  def mtype(m : Symbol, cls : Type, ct: CT): MethodCT = {
    val posMTyp = ct.methods.find(ftyp => (ftyp.name == m) && (ftyp.cls == cls) )
    posMTyp match {
      case None => ct.ext.toMap[Type,Type].get(cls)   match {
        case None => ???
        case Some(superCls) =>  mtype(m, superCls, ct)
      }
      case Some(mTyp) => MethodCT(mTyp.cls, mTyp.name ,mTyp.params, mTyp.ret)
    }
  }

  def extend(cls : Type, ct : CT) : Type = {
    ct.ext.toMap[Type, Type].get(cls) match {
      case None => ???
      case Some(superCls) => superCls // or return ext(cls, superCls)
    }
  }

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      try{
        val (t, sol_) = typecheckRec(root, Map(), CT())
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

  def typecheckRec(e: Node_[Result], ctx: Ctx, ct : CT): Result = {
  val (t, cons, css) = typecheckStep (e, ctx, ct)
  val subcs = css.foldLeft (freshConstraintSystem) ((cs, res) => cs mergeSubsystem res)
  val cs = subcs addNewConstraints cons
  (cs applyPartialSolution t, cs.propagate)
  }

  def typecheckStep(e: Node_[Result], ctx: Ctx, ct: CT): StepResult = e.kind match {

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      ctx.get(x) match {
        case None => throw UnboundVariable(x, ctx)
        case Some(t) => (t, Seq(), Seq())
      }
    case FieldAcc =>
      val f = e.lits(0).asInstanceOf[Symbol] //symbol
    val (t, cs) = typecheckRec(e.kids(0), ctx, ct) //subsol
      (t, Seq(), Seq(cs))

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (te, cse) = typecheckRec(e.kids(0), ctx, ct)

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


}
