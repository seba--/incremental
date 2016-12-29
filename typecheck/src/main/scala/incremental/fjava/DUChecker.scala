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
case class CtorCT(cls: Type, args: List[Type]) extends CTcls[CtorCT] {
  def self = this
  def subst(s: CSubst) = CtorCT(cls, args)

}
case class FieldCT(cls: Type, field: Symbol, typ: Type) extends CTcls[FieldCT] {
  def self = this
  def subst(s: CSubst) = FieldCT(cls, field, typ)
}
case class MethodCT(cls: Type, name: Symbol, params: List[Type], ret: Type) extends CTcls[MethodCT] {
  def self = this
 def subst(s: CSubst) = MethodCT(cls, name,  params, ret)
}


case class CT (
                ext: Set[ExtCT] = Set(),
                ctorParams: Set[CtorCT] = Set(),
                fields: Set[FieldCT] = Set(),
                methods: Set[MethodCT] = Set())


case class UnboundVariable(x: Symbol, ctx: Map[Symbol, Type]) extends RuntimeException
case class UndefinedSuper(cls: Type) extends RuntimeException
case class UndefinedCClass(name : Symbol) extends RuntimeException
case class UndefinedField(cls: Type, name: Symbol) extends RuntimeException
case class UndefinedMethod(cls: Type, name: Symbol) extends RuntimeException
case class MethodWrongArity(cls: Type, name: Symbol, expectedArity: Int) extends RuntimeException
case class UndefinedCTor(cls: Type) extends RuntimeException
case class CTorWrongArity(cls: Type, expectedArity: Int) extends RuntimeException


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

  def field(f : Symbol, cls : Type, ct: CT): Option[Type] = {
    val posFTyp = ct.fields.find(ftyp => (ftyp.field == f) && (ftyp.cls == cls) )
    posFTyp match {
      case None => ct.ext.find(extD => extD.cls == cls) match {
        case None => None
        case Some(superCls) => field(f, superCls.ext, ct)
      }
      case Some(fTyp) =>  Some(fTyp.typ)
    }
  }

  def mtype(m : Symbol, cls : Type, ct: CT): Option[List[Type]] = {
    val posMTyp = ct.methods.find(ftyp => (ftyp.name == m) && (ftyp.cls == cls) )
    posMTyp match {
      case None => ct.ext.find(extD => extD.cls == cls)  match {
        case None => None
        case Some(superCls) =>  mtype(m, superCls.ext, ct)
      }
      case Some(mTyp) => Some(mTyp.ret +: mTyp.params)
    }
  }

  def extend(cls : Type, ct : CT) : Option[Type] = {
    ct.ext.find(extD => extD.cls == cls) match {
      case None => None
      case Some (extT) => Some(extT.ext)
    }
  }

  def init(cls : Type, ct : CT) : Option[List[Type]] = { // return CtorCT
    ct.ctorParams.find(extD => extD.cls == cls) match {
      case None => None
      case Some(ctorTyp) => Some(ctorTyp.args)
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
        case ex: UndefinedSuper => Right(s"Undefined super class ${ex.cls}")
        case ex: UndefinedCClass => Right(s"Undefined CurrentClass for method ${ex.name}")
        case ex: UndefinedField => Right(s"Undefined field ${ex.name} in class ${ex.cls}")
        case ex: UndefinedMethod => Right(s"Undefined method ${ex.name} in class ${ex.cls}")
        case ex: MethodWrongArity => Right(s"Method ${ex.cls}.${ex.name} should have arity ${ex.expectedArity}")
        case ex: UndefinedCTor => Right(s"Undefined Constructor ${ex.cls}")
        case ex: CTorWrongArity => Right(s"Constructor ${ex.cls} should have arity ${ex.expectedArity}")
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
      val fTyp = field(f,t, ct).getOrElse(throw new UndefinedField(t, f))
      (fTyp, Seq(), Seq(cs))

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (te, cse) = typecheckRec(e.kids(0), ctx, ct)
      val mtyp = mtype(m, te, ct).getOrElse(throw new UndefinedMethod(te, m))
      val params = mtyp.size - 1 // -1 because of return type
      val args = e.kids.seq.size - 1 // -1 because of receiver expression
      if (params != args)
        throw new MethodWrongArity(te, m, args)
      var cs = Seq[CS]()
      var cons = Seq[Constraint]()
      for (i <- 1 until params) {
        val (ti, csi) = typecheckRec(e.kids(i), ctx, ct)
        cons = cons :+ Subtype(ti,  mtyp(i))
        cs = cs ++ Seq(csi)
      }
      (mtyp.head, cons, Seq(cse) ++ cs)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      val ctor = init(c, ct).getOrElse(throw new UndefinedCTor(c)
      var cons = Seq[Constraint]()
      var cs = Seq[CS]()
      if (e.kids.seq.size != ctor.size)
        throw new CTorWrongArity(c, e.kids.seq.size)
      for (i <- 0 until e.kids.seq.size) {
        val (ti, csi) = typecheckRec(e.kids(i), ctx, ct)
        cons =  cons :+ Subtype(ti, ctor(i))
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

      var cons = Seq[Constraint]()
      var ctxP = ctx

      for ((x,xC) <- params) {
        ctxP = ctxP + (x -> xC)
      }

      val cls = ctxP.get(CURRENT_CLASS).orElse(throw new UndefinedCClass(m))

      val extD = extend(cls, ct).getOrElse(throw  new UndefinedSuper(cls))
      mtype(m, extD, ct) match {
         case None => cons
         case Some(mtyp) => cons = Equal(mtyp.last, retT) +: AllEqual(mtyp.drop(0) , params.toMap.values.toList) +: cons
       }

      val (bodyT, csb) = typecheckRec(e.kids(0), ctxP, ct)

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
      val ctorsup = init(sup, ct).getOrElse(throw new UndefinedCTor(sup))
      // constructor initializes all local  or super class fields
      val fieldSupInitCons = AllEqual(ctorsup, ctor.superParams.values.toList)
      // constructor provides correct arguments to super constructor

      //add the super class in CS solver
      (c, Seq(fieldSupInitCons), cs)

    case ProgramM =>

      var cs= Seq[CS]()

      var removeCons = Seq[Constraint]()
      var ctNew = CT()
      var ctxNew = Map()

      // remove class requirements
      for (cls <- e.kids.seq) {
        val cname = cls.lits(0).asInstanceOf[CName]
        val sup = cls.lits(1).asInstanceOf[CName]
        val ctor = cls.lits(2).asInstanceOf[Ctor]
        val fields = cls.lits(3).asInstanceOf[Seq[(Symbol, CName)]].toMap
        val methods = cls.kids.seq

        val newMethods = methods.map { mtyp =>
          MethodCT(cname, mtyp.lits(1).asInstanceOf[Symbol],mtyp.lits(2).asInstanceOf[List[(Symbol, CName)]].map(_._2), mtyp.lits(0).asInstanceOf[CName] )}
       //  ctxNew = ctx + (CURRENT_CLASS -> cname) + ('this -> c) + ('other -> CName('Zero))
        ctNew = CT(ctNew.ext + ExtCT(cname, sup), ctNew.ctorParams + CtorCT(cname, ctor.superParams.values.toList ++ ctor.fields.values.toList ), ctNew.fields ++ fields.map(ftyp => FieldCT(cname, ftyp._1, ftyp._2)) , ctNew.methods ++ newMethods )
      }

      for (i <- 0 until e.kids.seq.size) {
        val (ct, csi) = typecheckRec(e.kids(i), ctx, ctNew)
        cs = cs ++ Seq(csi)
      }


      (ProgramOK, Seq(), cs)

  }


}
