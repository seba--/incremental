package incremental.fjava

/**
 * Created by lirakuci on 10/26/16.
 */

import constraints.Statistics
import constraints.fjava.CSubst.CSubst
import constraints.fjava.{ConstraintSystem, _}
import incremental.Node._
import incremental.{Node_, Util}
/**
 * Created by lirakuci on 24/10/16.
 */

case class UnboundVariable(x: Symbol, ctx: Map[Symbol, Type]) extends RuntimeException
case class UndefinedSuper(cls: Type) extends RuntimeException
case class UndefinedCClass(name : Symbol) extends RuntimeException
case class UndefinedField(cls: Type, name: Symbol) extends RuntimeException
case class UndefinedMethod(cls: Type, name: Symbol) extends RuntimeException
case class MethodWrongArity(cls: Type, name: Symbol, expectedArity: Int) extends RuntimeException
case class UndefinedCTor(cls: Type) extends RuntimeException
case class NewWrongArity(cls: Type, expected: Int, actual: Int) extends RuntimeException


case class DUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new DUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}


abstract class DUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  case class Entry(supr: CName, ctor: Ctor, fields: Map[Symbol, Type], methods: Map[Symbol, (Seq[Type], Type)])

  type CT = Map[Type, Entry]

  type Ctx = Map[Symbol, Type]

  type StepResult = (Type, Seq[Constraint], Seq[CS])

  type TError = String

  type Result = (Type, CS)

  val CURRENT_CLASS = '$current

  def field(f : Symbol, cls : Type, ct: CT): Option[Type] =
    for {
      clazz <- ct.get(cls)
      t <- clazz.fields.get(f).orElse(field(f, clazz.supr, ct))
    } yield t

  def mtype(m : Symbol, cls : Type, ct: CT): Option[(Seq[Type], Type)] =
    for {
      clazz <- ct.get(cls)
      t <- clazz.methods.get(m).orElse(mtype(m, clazz.supr, ct))
    } yield t

  def extend(cls : Type, ct : CT) : Option[Type] =
    for {
      clazz <- ct.get(cls)
    } yield clazz.supr

  def init(cls : Type, ct : CT) : Option[Seq[Type]] = { // return CtorCT
    if (cls == CName('Object)) {
      Some(List())
    }
    else for {
      clazz <- ct.get(cls)
    } yield clazz.ctor.allArgTypes
  }

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      try{
        val (t, sol_) = typecheckRec(root, Map(), Map())
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
        case ex: NewWrongArity => Right(s"Constructor call for ${ex.cls} should have arity ${ex.expected}, got ${ex.actual}")
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
      //check receiver
      val (te, cse) = typecheckRec(e.kids(0), ctx, ct)
      //match and check params with args
      val (params, ret) = mtype(m, te, ct).getOrElse(throw new UndefinedMethod(te, m))
      val args = e.kids.seq.tail
      if (params.size != args.size)
        throw new MethodWrongArity(te, m, args.size)
      var cs = Seq[CS]()
      var cons = Seq[Constraint]()
      for( (param, arg) <- params.zip(args)) {
        val (ti, csi) = typecheckRec(arg, ctx, ct)
        cons = Subtype(ti, param) +: cons
        cs = csi +: cs
      }
      (ret, cons, cse +: cs)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      val ctor = init(c, ct).getOrElse(throw new UndefinedCTor(c))
      var cons = Seq[Constraint]()
      var cs = Seq[CS]()
      if (ctor.size != e.kids.seq.size)
        throw new NewWrongArity(c, ctor.size, e.kids.seq.size)
      for ((param, arg) <- ctor.zip(e.kids.seq)) {
        val (ti, csi) = typecheckRec(arg, ctx, ct)
        cons = Subtype(ti, param) +: cons
        cs = csi +: cs
      }
      (c, cons, cs)

    case UCast =>
      val (t, cs) = typecheckRec(e.kids(0), ctx, ct)
      val c = e.lits(0).asInstanceOf[CName]
      (c, Seq(Subtype(t, c)), Seq(cs))

    case DCast =>
      val (t, cs) = typecheckRec(e.kids(0), ctx, ct)
      val c = e.lits(0).asInstanceOf[CName]
      (c, Seq(Subtype(c.asInstanceOf[Type], t)), Seq(cs)) //, NotEqual(c,  t)), Seq(cs))

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
      val cls = ctxP.get(CURRENT_CLASS) match {
        case None => throw new UndefinedCClass(m)
        case Some(cls) => cls
      }

      extend(cls, ct) match {
       case None => cons
       case Some(extD) => mtype(m, extD, ct) match {
         case None => cons
         case Some((mtyp, ret)) => cons = Equal(ret, retT) +: AllEqual(mtyp, params.unzip._2) +: cons
       }
      }

      val (bodyT, csb) = typecheckRec(e.kids(0), ctxP, ct)

      // body type is subtype of declared return type
      cons = Subtype(bodyT, retT) +: cons

        (MethodOK, cons, Seq(csb))

    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val ctor = e.lits(2).asInstanceOf[Ctor]
      val fields = e.lits(3).asInstanceOf[Seq[(Symbol, CName)]]

      var cs = Seq[CS]()

      // handle all methods, satisfying current-class reqs
      for (m <- e.kids.seq) {
        val (t, csi) = typecheckRec(m, ctx + (CURRENT_CLASS -> c) + ('this -> c), ct)
        cs = csi +: cs
      }
      val ctorsup = init(sup, ct).getOrElse(throw new UndefinedCTor(sup))
      // constructor initializes all local  or super class fields
      val fieldSupInitCons = AllEqual(ctorsup, ctor.superParams.values.toSeq)
      // constructor provides correct arguments to super constructor
      val fieldInitCons = AllEqual(fields.unzip._2, ctor.fields.values.toSeq)

      //add the super class in CS solver
      (c, Seq(fieldSupInitCons) ++ Seq(fieldInitCons), cs)

    case ProgramM =>

      var cs = Seq[CS]()

      var removeCons = Seq[Constraint]()
      var ctxNew = Map()

      var classes = Seq[Node_[Result]]()
      def findClasses(node: Node_[Result]): Unit = {
        if (node.kind == ProgramM)
          node.kids.seq.foreach(findClasses)
        else if (node.kind == ClassDec)
          classes = node +: classes
      }
      findClasses(e)

      var classTable: CT = Map()

      // remove class requirements
      for (cls <- classes) {
        val cname = cls.lits(0).asInstanceOf[CName]
        val sup = cls.lits(1).asInstanceOf[CName]
        val ctor = cls.lits(2).asInstanceOf[Ctor]
        val fields = cls.lits(3).asInstanceOf[Seq[(Symbol, CName)]].toMap
        val methods = cls.kids.seq

        val mthds = methods.map { mtyp =>
          (mtyp.lits(1).asInstanceOf[Symbol],(mtyp.lits(2).asInstanceOf[Seq[(Symbol, CName)]].unzip._2, mtyp.lits(0).asInstanceOf[CName]))
        }
        classTable = classTable + (cname -> Entry(sup, ctor, fields, Map(mthds:_*)))
      }

      for (cls <- classes) {
        val (ct, csi) = typecheckRec(cls, ctx, classTable)
        cs = csi +: cs
      }


      (ProgramOK, Seq(), cs)

  }


}
