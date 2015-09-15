package incremental.FJava

import constraints.{CVar, Statistics}
import constraints.subtype._
import incremental.{Node_, Util}
import incremental.Node._


/**
 * Created by lirakuci on 3/2/15.
 */


case class FieldName(x: Symbol)
case class Param(x: Symbol)


abstract class BUChecker[CS <: ConstraintSystem[CS]] extends TypeChecker[CS] {

  import csFactory._

  type TError = String

  type Methods = Map[Symbol, (Type, Map[Symbol, Type], Type)]

  type Fields = Map[Symbol, Type]

  type ClassDecl = (Type, Fields, Methods) //(Type, Fields, Methods)

  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Type, ClassDecl]

  //def Subtype(C: Type, D: Type, creqs : CReqs): Boolean = {
    //for((C, cld) <- creqs) {
      //if (cld._1 == D)
        //true
    //}
    //false
  //}

  def getX(t: Type, reqs: Reqs): Symbol = {
    var x: Symbol = 'a
    for ((x1, t1) <- reqs) {
      if (t1 == t) x = x1
    }
    x
  }


  type StepResult = (Type, Reqs, CReqs, Seq[Constraint])
  type Result = (Type, Reqs, CReqs, CS)

  def typecheckImpl(e: Node): Either[Type, TError] = {
    val root = e.withType[Result]

    Util.timed(localState -> Statistics.typecheckTime) {
      root.visitUninitialized { e =>
        val (t, reqs, creqs, cons) = typecheckStep(e)
        val subcs = e.kids.seq.foldLeft(freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
        val cs = subcs addNewConstraints cons
        val reqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)
        //  e.typ = (cs applyPartialSolution t, reqs2, creqs, sig, cs.propagate)
        var creqs2: CReqs = Map()
       // var typ = Seq[Type]()
        for ((tc, cld) <- creqs) {
          val fld = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](cld._2, p => p._2)
          var meth = Map[Symbol, (Type, Map[Symbol, Type], Type)]()
          for ((sm, body) <- cld._3) {
            val m = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](body._2, p => p._2)
            meth = meth ++ Map(sm ->(body._1, m, body._3))
          }
          creqs2 = creqs2 - tc + (tc.subst(cs.substitution) ->(cld._1, fld, meth))
        } //change the merging of the class declaration
        //val fld = cld._2
        //val creqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)//does not work unification in the class declaration inside
    //  val creqs3 = cs.applyPartialSolutionIt[(Type), Seq[Type], Type](creqs, p => p)
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

    case This =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = CName(x)
      (X, Map('This -> X), Map(), Seq())

    case Fields =>
      val f = e.lits(0).asInstanceOf[Symbol] //symbol
    val (t, reqs, creqs, _) = e.kids(0).typ //subsol
    val U = freshCName()
      val ct: ClassDecl = (null, Map(f -> U), Map())

      val (cons, mcreqs) = mergeCReqMaps(creqs, Map(t -> ct))
      (U, reqs, mcreqs, cons) //subsol

    case Invk =>
      val m = e.lits(0).asInstanceOf[Symbol]
      val (t0, reqs0, creqs0, _) = e.kids(0).typ
      val C = freshCName()

      var reqss: Seq[Reqs] = Seq(reqs0)
      var creqss: Seq[CReqs] = Seq(creqs0)
      var param = Map[Symbol, Type]()
      for (i <- 1 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids.seq(i).typ

        val xi = freshParam().x
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
        param += (xi -> ti)
      }
      val cld: ClassDecl = (null, Map(), Map(m ->(C, param, C)))

      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)
      val (mcCons, mcreqs) = mergeCReqMaps(creqs, Map(t0 -> cld))

      (C, mreqs, mcreqs, mcons ++ cCons ++ mcCons)

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      val U = freshCName()
      var reqss = Seq[Reqs]()
      var creqss = Seq[CReqs]()
      var fields = Map[Symbol, Type]()
      for (i <- 0 until e.kids.seq.size) {
        val (ti, subreqs, subcreqs, _) = e.kids.seq(i).typ
       // val  fi = getX(ti, subreqs) // ========> see again, it should be fresh field
        val fi = freshField().x
        reqss = reqss :+ subreqs
        creqss = creqss :+ subcreqs
        fields += (fi -> ti)
      }

      val cld: ClassDecl = (null, fields, Map())

      val (mcons, mreqs) = mergeReqMaps(reqss)
      val (cCons, creqs) = mergeCReqMaps(creqss)
      val (mcCons, mcreqs) = mergeCReqMaps(creqs, Map(c -> cld))
      (c, mreqs, mcreqs, mcons ++ cCons ++ mcCons)

    case DCast =>
      val (t, reqs, creqs, _) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]

      (c, reqs, creqs, Seq((NotEqConstraint(t, c)))) //cs.never(EqConstraint(t,c)

    case UCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs, _) = e.kids(0).typ
      val cld : ClassDecl = (c, Map(), Map())
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
      var restReqs = reqs
      var cons = Seq[Constraint]()
      val params = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]
      var fld = Map[Symbol,Type]()
     // var superT : Type = CName('Object)
     // var actual : Type = CName('Bool)
      var cldMC = Map[Symbol, (Type, Map[Symbol, Type], Type)]()
      var cldMD = Map[Symbol, (Type, Map[Symbol, Type], Type)]()
      var sparam = params.toMap
      var sig = Signature( retT, m, sparam, e0)

      val th = freshCName()

      var cCreqs = creqs


      for ((x, xC) <- params) {
        reqs.get(x) match {
          case None => restReqs = restReqs
          case Some(typ) =>
            restReqs = restReqs - x
            cons = Subtype(typ, xC) +: cons //not sure if it should be the other way round
        }
      }

      for ((c, cldC) <- creqs) {
        //superT = cldC._1
        // actual = c
        cldC._3.get(m) match {
          case None => sig = sig
          case Some(cldM1) =>
            var paramM = cldM1._2
            if (paramM.size == sparam.size) {
              for ((x, xC) <- params)
                for ((x, xC) <- params) {
                  for ((p, re) <- paramM) {
                    reqs.get(x) match {
                      case None => paramM = paramM
                      case Some(treq) =>
                        restReqs = restReqs - x
                        cons = Subtype(xC, treq) +: cons
                    }
                    cons = Subtype(xC, re) +: cons
                    paramM = paramM - p
                  }
                }
            }
            else cons = Equal(TNum, TString) +: cons


            cons = Equal(e0, cldM1._3) +: cons
            if (paramM.isEmpty) cldMC = cldC._3 - m
            else {
              cldMC = cldC._3 - m + (m ->(retT, paramM, e0))
              if (cldC._1 != null) {
                creqs.get(cldC._1) match {
                  case None => cCreqs = cCreqs // do the reassigment for the super type, if tis does not exist = fresh UCName
                   case Some(cldD) =>
                    cldD._3.get(m) match {
                      case None => cldMD = cldD._3
                      case Some(mD) =>
                        cldMD = cldD._3 - m + (m ->(retT, paramM, e0))
                        cCreqs = cCreqs - cldC._1 + (cldC._1 ->(cldD._1, cldD._2, cldMD))
                    }
                }
              }
            }

            cCreqs = cCreqs - c + (c ->(cldC._1, cldC._2, cldMC))
            sig = Signature(retT, m, params.toMap, e0)
            if (fld.isEmpty && cldMC.isEmpty) cCreqs = cCreqs - c
        }
       restReqs.get('this) match {
         case None => restReqs = restReqs
         case Some(typ1) =>
           cons = Equal(c, typ1) +: cons
            restReqs = restReqs - 'this
            //cCreqs = cCreqs - typ1
           // for (Equal(x, y) <- cons) {
             // if (x == typ1)
               // cCreqs = cCreqs - y
           }

        //}
      }



     // if ((e0 == retT) || (Subtype(retT, e0, cCreqs))) cons = cons
   //   else cons = EqConstraint(TNum, TString) +: cons

      (sig, restReqs, cCreqs, cons )//:+ Subtype(retT, e0))

    case ClassDec =>
      val c = e.lits(0).asInstanceOf[CName]
      val sup = e.lits(1).asInstanceOf[CName]
      val fields = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]
      val field = fields.toMap
      var sig = Map[Symbol, (Type, Map[Symbol, Type], Type)]()
      var restReq = Seq[Reqs]()
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()
      var cldM = Map[Symbol, (Type, Map[Symbol, Type], Type)]()
   //   var param = Seq[(Symbol, Type)]()
      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, _) = e.kids.seq(i).typ
        restReq = restReq :+ req
        restCreq = restCreq :+ creq

        val Signature(ret, m, params, bod) = t.asInstanceOf[Signature]
        sig = sig ++ Map(Signature(ret, m, params, bod).meth -> (Signature(ret, m, params, bod).returnTyp, Signature(ret, m, params, bod).parameters, Signature(ret, m, params, bod).body))
      }

      var (mcons, mreqs) = mergeReqMaps(restReq)
      var (mCcons, mcreqs) = mergeCReqMaps(restCreq)

      cons = cons ++ mcons ++ mCcons

    //  cons = Subtype(c, sup) +: cons

      var fieldn = Map[Symbol, Type]()
      var creq = mcreqs

      var typ = Seq[Type]()

      mcreqs.get(c) match {
        case None =>
        case Some(cld) =>
          cons = Equal(sup, cld._1) +: cons

          fieldn = cld._2
          cldM = cld._3
          if (fieldn.size == field.size) {
            for ((f, typ1) <- field) {
              for ((f2, typ2) <- cld._2) {
                fieldn = fieldn - f2
                cons = Equal(typ1, typ2) +: cons
              }
            }
          }
          else cons =  Equal(TNum, TString) +: cons

          for ((m, cldm) <- cld._3) {
            var mparam = cldm._2
            sig.get(m) match {
              case None =>
                cldM = cldM
              case Some(mSig) =>
                cons = Subtype(mSig._1, cldm._1) +: cons // unify the return type
                cons = Equal(cldm._3, mSig._3) +: cons // unify the body
              var param = mSig._2

             //   creq.get()
                if (param.size == mparam.size) { // unify equaly-named parameters
                for ((x, xC) <- param) {
                  for ((p, tParam) <- mparam) {
                    mparam = mparam - p
                    cons = Subtype(xC, tParam) +: cons
                  }
                }
                }
                else cons =  Equal(TNum, TString) +: cons

                if (mparam.isEmpty) cldM = cldM - m
                else cldM = cldM - m + (m ->(mSig._1, mparam, mSig._3))
            }
          }
           if (fieldn.isEmpty && cldM.isEmpty)
            creq = creq - c
          else
            creq = creq - c ++ Map(c ->(cld._1, fieldn, cldM))

      }
     // for ((c,cldC) <- creq) {
     //   if (cldC._2.isEmpty && cldC._3.isEmpty) creq = creq - c
    //  }

      mreqs.get('this) match {
        case None => mreqs = mreqs
        case Some(typ1) =>
            cons = Equal(c, typ1) +: cons
          mreqs = mreqs - 'this
          creq = creq - typ1
          for (Equal(x, y) <- cons){
            if ( x == typ1)
              creq = creq -y
          }
      }


      if (sup != CName('Object)) {
   val cldSup: ClassDecl = (null, Map(), Map())
   var (sMCons, sMCReqs) = mergeCReqMaps(creq, Map(sup -> cldSup))
  creq = sMCReqs
  cons = cons ++ sMCons
}
      else{ creq = creq
  cons = cons }

      ( c, mreqs, creq, cons)

    case ProgramM =>

      var restReq = Seq[Reqs]()
      var restCreq = Seq[CReqs]()
      var cons = Seq[Constraint]()
      var cls = Seq[Type]()
      for (i <- 0 until e.kids.seq.size) {
        val (t, req, creq, _) = e.kids.seq(i).typ
        restReq = restReq :+ req
        restCreq = restCreq :+ creq
        cls = cls :+ t

      }
      var (mcons, mreqs) = mergeReqMaps(restReq)
      var (mCcons, mcreqs) = mergeCReqMaps(restCreq)

      for (t <- cls) {
        mcreqs.get(t) match {
          case None =>
         mcreqs = mcreqs
          case Some(_) =>
            mcreqs = mcreqs - t
        }
      }
 cons = cons ++ mcons ++ mCcons
      //for (EqConstraint(x, y) <- cons){
        //if ( Subtype(x,y, mcreqs))
       // cons = cons.dro
     // }
        (ProgramOK, mreqs, mcreqs, cons)

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
          val Xmeet = gen.freshUVar(false)
          mcons = Meet(Xmeet, Set(r1, r2)) +: mcons
          mreqs += x -> Xmeet
      }
    (mcons, mreqs)
  }

  private val cinit: (Seq[Constraint], CReqs) = (Seq(), Map())

  def mergeCCld(cld1: ClassDecl, cld2: ClassDecl): (Seq[Constraint], ClassDecl) = {
    val wasF = cld1._2
    val wasM = cld1._3
    var mcons = Seq[Constraint]()
    var cldf = wasF
    var cldm = wasM
    for ((f, typ) <- cld2._2)
      for ((f1, typ1) <- wasF) {/// check the merge req in the subtyping and do with the mstch and get, if it mathced we do join if not jsut add .....
        val Fjoin = gen.freshUVar(false)
        mcons = Join(Fjoin, Set(typ1, typ)) +: mcons
      }
    for ((m, mbody) <- cld2._3)
      wasM.get(m) match {
        case None => cldm += (m -> mbody) // mdoby = return type + list of parameters
        case Some(mbody1) =>
          val Rmeet = gen.freshUVar(false)
          val Bmeet = gen.freshUVar(false)
          mcons = Meet(Rmeet, Set(mbody1._1, mbody._1)) +: Meet(Bmeet, Set(mbody1._3, mbody._3)) +: mcons
          var params = mbody1._2
          for ((p, t) <- mbody._2)
            mbody1._2.get(p) match {
              case None => params += (p -> t)
              case Some(t2) =>
                val Pjoin = gen.freshUVar(false)
                mcons = Join(Pjoin, Set(t, t2)) +: mcons
            }
          cldm = Map(m ->(mbody1._1, params, mbody1._3))
      }
    val cld: ClassDecl = (cld1._1, cldf, cldm)

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
        case Some(cld1) => mcreqs += (t -> mergeCCld(cld1, cld2)._2)
          mcons = mergeCCld(cld1, cld2)._1 ++ mcons
        //mcons = EqConstraint(cld1.c, cld2.c) +: mcons
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
