package incremental.fjava

import constraints.{CVar, Statistics}
import constraints.fjava._
import constraints.fjava.impl
import incremental.{NodeKind, Node_, Util}
import incremental.Node._

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

  type ClassDecl = (List[Type], Fields, Methods) //(Type, Fields, Methods)

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
        var creqs2 = creqs
        for ((t, cld) <- creqs) {
          val c = t.subst(cs.substitution)
          creqs2 = creqs2 - t + (c -> cld)
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
      val ct: ClassDecl =(List(), Map(f -> U), Map())


      val (cons, mcreqs) = mergeCReqMaps(creqs, Map(t -> ct))
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
      val cld: ClassDecl = (List(), Map(), Map(m ->(U, param)))

      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)
      val (mcCons, mcreqs) = mergeCReqMaps(creqs, Map(te -> cld))

      (U, mreqs, mcreqs, mcons ++ cCons ++ mcCons ++ cons)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      val U = freshCName()
      var reqss = Seq[Reqs]()
      var creqss = Seq[CReqs]()
      var cons = Seq[Constraint]()
      for (i <- 0 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids.seq(i).typ
        val Ui = freshCName()
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
        cons = Subtype(ti, Ui) +: cons
      }
      val cld: ClassDecl = (List(), Map(), Map())

      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)
      val (mcCons, mcreqs) = mergeCReqMaps(creqs, Map(c -> cld))
      (c, mreqs, mcreqs, mcons ++ cCons ++ mcCons ++cons)

    case DCast =>
      val (t, reqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, creqs, Seq((NotEqual(t, c))))

    case UCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs, _) = e.kids(0).typ
      val cld : ClassDecl = (List(c), Map(), Map())
      var (mcons, mcreqs) = mergeCReqMaps(creqs, Map(t -> cld))
      (c, reqs, mcreqs, mcons)

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
      val fields = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]
      val field = fields.toMap
      var cls = List[Type]()
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()

      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, _) = e.kids.seq(i).typ
        restCreq = restCreq :+ creq
        cons = cons :+ Equal(c, t.asInstanceOf[MethodOK].in)
      }
      var cr : CReqs = Map()
      val scld : ClassDecl = (List(), Map(), Map())
      if (sup == CName('Object)) {
        var (mcons, mcreqs) = mergeCReqMaps(restCreq)
        cr = mcreqs
        cons = cons ++ mcons
      }
      else {
        var (mcons, mcreqs) = mergeCReqMaps(restCreq)
        val (mCons, mCreqs) = mergeCReqMaps(mcreqs, Map(sup -> scld))
        cons = cons ++ mcons ++ mCons
        cr = mCreqs
      }
      for ((c, cld)<- cr){
        if (cld._2.isEmpty && cld._3.isEmpty && cld._1 == CName('Object))
        cr = cr - c
      }

      cons = Extend(c, sup) +: cons
      (c, Map(), cr, cons) //tc

    case ProgramM =>
      var CT = Map[Type,(List[Type], Map[Symbol, Type], Map[Symbol, (Type, List[Type])])]()
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()
      for (i <- 0 until e.kids.seq.size) {
        val c = e.kids(i).lits(0).asInstanceOf[CName]
        val sup = e.kids(i).lits(1).asInstanceOf[CName]
        val fields = e.kids(i).lits(2).asInstanceOf[Seq[(Symbol, Type)]]
        var methods = Map[Symbol, (Type, List[Type])]()
        for (j <- 0 until e.kids(i).kids.seq.size) {
          val (c0, reqs, creqs, _)= e.kids(i).kids.seq(j).typ
          cons = cons :+ Equal(c, c0.asInstanceOf[MethodOK].in)
          val retT = e.kids(i).kids(j).lits(0).asInstanceOf[CName]
          val m = e.kids(i).kids(j).lits(1).asInstanceOf[Symbol]
          val params = e.kids(i).kids(j).lits(2).asInstanceOf[Seq[(Symbol, Type)]]
          var par = List[Type]()
          for((t, p) <- params.toMap)
            par = par :+ p
        methods = methods + (m -> (retT, par))
          restCreq = restCreq :+ creqs
        }
        CT =  CT + (c -> (List(sup), fields.toMap, methods))
      }
       val  subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)

      var (mCcons, mcreqs) = mergeCReqMaps(restCreq)
      cons = cons ++ mCcons //++ ctons

    //  val cs = subcs addNewConstraints cons
     val (ct, ctcons) =  remove(CT, mcreqs, subcs)

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

  private def remove(CT : Map[Type, ClassDecl], creq : CReqs, cs :CS) : (CReqs, Seq[Constraint]) = {
    var cr = creq
    var cons = Seq[Constraint]()
    var ct = CT
    for ((c, cld) <- creq) {
      var stype = cld._1
      var fields = cld._2
      var methods = cld._3
      CT.get(c) match {
        case None => cr
        case Some(clsT) =>
          if (cld._1.length > 0 && clsT._1.length > 0 )
         cons = cons :+ Equal(cld._1.head, clsT._1.head)
          for ((f, typ) <- cld._2){
            clsT._2.get(f) match {
              case None => fields = fields
              case Some(typ2) =>
                cons = cons :+ Equal(typ, typ2)
                fields = fields - f
            }
          }
          for ((m, rt) <- cld._3){
            clsT._3.get(m) match {
              case None => methods = methods
              case Some(rt2) =>
                cons = cons :+ Equal(rt._1, rt2._1)
                if (! rt._2.isEmpty) {
                  for (i <- 0 until rt._2.length)
                    cons = cons :+ Equal(rt._2(i), rt2._2(i))
                }
                methods = methods - m
            }
          }

      }
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
