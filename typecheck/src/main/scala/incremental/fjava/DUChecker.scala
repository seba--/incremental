package incremental.fjava

/**
 * Created by lirakuci on 10/26/16.
 */

import com.typesafe.config.ConfigException.Null
import constraints.Statistics
import constraints.fjava.CSubst.CSubst
import constraints.fjava.{ConstraintSystem, _}
import incremental.Node._
import incremental.{Node_, Util, pcf}
/**
 * Created by lirakuci on 24/10/16.
 */

import incremental.fjava.latemerge.Condition.trueCond

trait CTcls[T <: CTcls[T]] {
  def self: T
}

case class ExtCT(cls: Type, ext: Type) extends CTcls[ExtCT] {
  def self = this
 def subst(s: CSubst) =  ExtCT(cls, ext)
  }
case class CtorCT(cls: Type, args: Seq[Type]) extends CTcls[CtorCT] {
  def self = this
  def subst(s: CSubst) = CtorCT(cls, args)

}
case class FieldCT(cls: Type, field: Symbol, typ: Type) extends CTcls[FieldCT] {
  def self = this
  def subst(s: CSubst) = FieldCT(cls, field, typ)
}
case class MethodCT(cls: Type, name: Symbol, params: Seq[Type], ret: Type) extends CTcls[MethodCT] {
  def self = this
 def subst(s: CSubst) = MethodCT(cls, name,  params, ret)
}


case class CT (
                ext: Set[ExtCT] = Set(),
                ctorParams: Set[CtorCT] = Set(),
                fields: Set[FieldCT] = Set(),
                methods: Set[MethodCT] = Set())


case class UnboundVariable(x: Symbol, ctx: Map[Symbol, Type]) extends RuntimeException


case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}


abstract class DUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type Ctx = Map[Symbol, Type]

  type StepResult = (Type, Seq[Constraint], Seq[CS])

  type TError = String

  type Result = (Type, CS)

  val CURRENT_CLASS = '$current

  def field(f : Symbol, cls : Type, ct: CT): Seq[Type] = {
    val posFTyp = ct.fields.find(ftyp => (ftyp.field == f) && (ftyp.cls == cls) )
    posFTyp match {
      case None => ct.ext.find(extD => extD.cls == cls) match {
        case None => Seq()
        case Some(superCls) => field(f, superCls.ext, ct)
      }
      case Some(fTyp) =>  Seq(fTyp.typ)
    }
  }

  def mtype(m : Symbol, cls : Type, ct: CT): Seq[Type] = {
    val posMTyp = ct.methods.find(ftyp => (ftyp.name == m) && (ftyp.cls == cls) )
    posMTyp match {
      case None => ct.ext.find(extD => extD.cls == cls)  match {
        case None => Seq()
        case Some(superCls) =>  mtype(m, superCls.ext, ct)
      }
      case Some(mTyp) => mTyp.params ++ Seq(mTyp.ret)
    }
  }

  def extend(cls : Type, ct : CT) : Seq[Type] = {
    ct.ext.find(extD => extD.cls == cls) match {
      case None => Seq()
      case Some(superCls) => Seq(superCls.ext) // or return ext(cls, superCls)
    }
  }

  def init(cls : Type, ct : CT) : Seq[Type] = { // return CtorCT
    ct.ctorParams.find(extD => extD.cls == cls) match {
      case None => Seq()
      case Some(ctorTyp) => ctorTyp.args
    }
  }

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      try{
        val (t, sol_) = typecheckRec(root, Map(), CT())
        val sol = sol_.tryFinalize
      if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type ${t}")
      else
        Left(t)

      } catch {
        case ex: UnboundVariable => Right(s"Unbound variable ${ex.x} in context ${ex.ctx}")
      }
    }
  }

  def typecheckRec(e: Node_[Result], ctx: Ctx, ct : CT): Result = {
  val (t, cons, css) = typecheckStep (e, ctx, ct)
  val subcs = css.foldLeft (freshConstraintSystem) ((cs, res) => cs mergeSubsystem res)
  val cs = subcs addNewConstraints cons
    val csF = if (e.kind != ClassDec) cs else {
      // add extends to constraint system
      val c = e.lits(0).asInstanceOf[GroundType]
      val sup = e.lits(1).asInstanceOf[GroundType]
      cs.extendz(c, sup)
    }
  (t, csF.propagate)
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
      val fTyp = field(f,t, ct)
      (fTyp.head, Seq(), Seq(cs))

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (te, cse) = typecheckRec(e.kids(0), ctx, ct)
      val mtyp = mtype(m, te, ct)
      var cs = Seq[CS]()
      var cons = Seq[Constraint]()
      for (i <- 1 until e.kids.seq.size) {
        val (ti, csi) = typecheckRec(e.kids(i), ctx, ct)
        cons = cons :+ Subtype(ti,  mtyp.toList(i))
        cs = cs ++ Seq(csi)
      }
      (mtyp.last , cons, Seq(cse) ++ cs)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      val ctor = init(c, ct)
      var cons = Seq[Constraint]()
      var cs = Seq[CS]()
      for (i <- 0 until e.kids.seq.size) {
        val (ti, csi) = typecheckRec(e.kids(i), ctx, ct)
        cons =  cons :+ Subtype(ti,ctor.toList(i))
        cs = cs ++ Seq(csi)
      }
      (c, cons, cs)

    case UCast =>
      val (t, cs) = typecheckRec(e.kids(0), ctx, ct)
      val c = e.lits(0).asInstanceOf[CName]

      (c, Seq(Subtype(t, c)), Seq(cs))

    case DCast =>
      val (t, cs) = typecheckRec(e.kids(0), ctx, ct)
      val c = e.lits(0).asInstanceOf[CName]

      (c, Seq(Subtype(c.asInstanceOf[Type], t), NotEqual(c,  t)), Seq(cs))

    case SCast =>
      val (t, cs) = typecheckRec(e.kids(0), ctx, ct)
      val c = e.lits(0).asInstanceOf[CName]

      (t, Seq(NotSubtype(c, t), NotSubtype(t, c), StupidCastWarning(t, c)), Seq(cs))

    case MethodDec =>
      val retT = e.lits(0).asInstanceOf[CName] // return type
      val m = e.lits(1).asInstanceOf[Symbol] // method name
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, CName)]]

      var ctMO = ct
      var ctxP = ctx

      for ((x,xC) <- params) {
        ctxP = ctxP + (x -> xC)
      }
      val cls = ctxP.get(CURRENT_CLASS) match {
        case None => CName('Object)
        case Some(cls) => cls
      }

      extend(cls, ct).headOption match {
       case None => ct
       case Some(extD) => mtype(m, extD , ct).headOption match {
         case None => ct
         case Some(mtyp) => ctMO = ct.copy(methods = ct.methods.filter(msupTyp => (msupTyp.name== m && msupTyp.cls == extD)) ++ Seq(MethodCT(extD, m, params.toMap.values.toSeq, retT)))
       }
      }

      //overide the signature of the method in super types

      val (bodyT, csb) = typecheckRec(e.kids(0), ctxP, ctMO)
      var cons = Seq[Constraint]()

      // body type is subtype of declared return type
      cons = Subtype(bodyT, retT) +: cons

        (MethodOK, cons, Seq(csb))

    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val ctor = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, CName)]].toMap

      var cs = Seq[CS]()
    //  var currentClassCons = Seq[Constraint]()

      // handle all methods, satisfying current-class reqs
      for (i <- 0 until e.kids.seq.size) {
        val (t, csi) = typecheckRec(e.kids(i), ctx + (CURRENT_CLASS -> c) + ('this -> c) + ('other -> CName('Zero)), ct)
        cs = cs ++ Seq(csi)
      }

      // constructor initializes all local  or super class fields
      val fieldSupInitCons = AllEqual(init(sup, ct).asInstanceOf[Seq[Type]], ctor.superParams.values.toList.asInstanceOf[Seq[Type]])
      // constructor provides correct arguments to super constructor

      //add the super class in CS solver
      (c, Seq(fieldSupInitCons), cs)

    case ProgramM =>

      var cs= Seq[CS]()

      var removeCons = Seq[Constraint]()
      var ctNew = CT()

      // remove class requirements
      for (cls <- e.kids.seq) {
        val cname = cls.lits(0).asInstanceOf[CName]
        val sup = cls.lits(1).asInstanceOf[CName]
        val ctor = cls.lits(2).asInstanceOf[Ctor]
        val fields = cls.lits(3).asInstanceOf[Seq[(Symbol, CName)]].toMap
        val methods = cls.kids.seq

        val newMethods = methods.map { mtyp =>
          MethodCT(cname, mtyp.lits(1).asInstanceOf[Symbol],mtyp.lits(2).asInstanceOf[Seq[(Symbol, CName)]].map(_._2), mtyp.lits(0).asInstanceOf[CName] )}

        ctNew = CT(ctNew.ext + ExtCT(cname, sup), ctNew.ctorParams + CtorCT(cname, ctor.superParams.values.toSeq ++ ctor.fields.values.toSeq ), ctNew.fields ++ fields.map(ftyp => FieldCT(cname, ftyp._1, ftyp._2)) , ctNew.methods ++ newMethods )
      }

      for (i <- 0 until e.kids.seq.size) {
        val (ct, csi) = typecheckRec(e.kids(i), ctx, ctNew)
        cs = cs ++ Seq(csi)
      }


      (ProgramOK, Seq(), cs)

  }


}
