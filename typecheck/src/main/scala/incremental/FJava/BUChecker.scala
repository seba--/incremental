package incremental.FJava

import java.util

import constraints.{equality, CVar, Statistics}
import constraints.equality._
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

  def Subtype(C: Type, D: Type): CReqs = {
    val cld: ClassDecl = (D, Map(), Map())
    Map(C -> cld)
  }

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

      // if (!reqs.isEmpty)
      //   Right(s"Unresolved variable requirements $reqs, type $t, unres ${cs.unsolved}")
      if (!creqs.isEmpty)
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

      val lcons = EqConstraint(CName('TNum), t1)
      val rcons = EqConstraint(CName('TNum), t2)
      val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
      val (mcCons, mCreqs) = mergeCReqMaps(creqs1, creqs2)

      (CName('TNum), mreqs, mCreqs, mcons :+ lcons :+ rcons)

    case Var =>
      val x = e.lits(0).asInstanceOf[Symbol]
      val X = freshCName()
      (X, Map(x -> X), Map(), Seq()) // Map(X -> cld), needed at some examples

    case This =>
      var th = e.lits(0).asInstanceOf[Symbol]
       th = 'This
      val Th = freshCName()
      (Th, Map(th -> Th), Map(), Seq())

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
        val  fi = getX(ti, subreqs) // ========> see again, it should be fresh field
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

      (c, reqs, creqs ++ Subtype(c, c), Seq((NotEqConstraint(t, c)))) //cs.never(EqConstraint(t,c)

    case UCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs, _) = e.kids(0).typ

      (c, reqs, creqs ++ Subtype(t, c), Seq())

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
      var superT : Type = null
      var cldMC = Map[Symbol, (Type, Map[Symbol, Type], Type)]()
      var cldMD = Map[Symbol, (Type, Map[Symbol, Type], Type)]()
      var sparam = params.toMap
      var sig = Signature( retT, m, sparam, e0)

      val th = freshCName()

      var cCreqs = creqs
      for ((c, cldC) <- creqs){
        cldC._3.get(m) match {
          case None => sig = sig
          case Some(cldM1) =>
           // restReqs = restReqs + (This -> c)
            var param = cldM1._2
            for (i <- 0 until(params.size)) {
              val x = params(i)._1
              val xC = params(i)._2
              reqs.get(x) match {
                case None => param = param
                case Some(treq) =>
                  for ((p, re) <- param) {
                    if (treq == re) param = param - p
                  }
                  restReqs = restReqs - x
                  cons = EqConstraint(xC, treq) +: cons
              }
            }
            cons = EqConstraint(e0, cldM1._3) +: cons
            if (param.isEmpty) cldMC = cldC._3 - m
            else {
              cldMC = cldC._3 - m + (m ->(retT, param, e0))
              if (cldC._1 != null) {
                creqs.get(cldC._1) match {
                  case None => superT = cldC._1
                  case Some(cldD) =>
                    cldD._3.get(m) match {
                      case None => cldMD = cldD._3
                      case Some(mD) =>
                        cldMD = cldD._3 - m + (m ->(retT, param, e0))
                        cCreqs = cCreqs - cldC._1 + (cldC._1 -> (cldD._1, cldD._2, cldMD))

                    }
                }
              }
            }
            reqs.get('This) match {
              case None => restReqs = restReqs
              case Some(typ1) =>
                cons = EqConstraint(c, typ1) +: cons
                restReqs = restReqs - 'This
            }
            cCreqs = cCreqs - c + (c -> (cldC._1, cldC._2, cldMC))

            sig = Signature(retT,m, params.toMap, e0)
            if (fld.isEmpty && cldMC.isEmpty) cCreqs = cCreqs - c
        }

      }
     // for ((c,cldC) <- creqs) {
       // if (cldC._2.isEmpty && cldC._3.isEmpty) cCreqs = cCreqs - c
      //}

      (sig, restReqs, cCreqs, cons :+ EqConstraint(e0, retT))


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

      var fieldn = Map[Symbol, Type]()
      var creq = mcreqs

      var typ = Seq[Type]()

      mreqs.get('This) match {
        case None => mreqs = mreqs
        case Some(typ2) =>
          cons = EqConstraint(c, typ2) +: cons
          typ = typ :+ typ2
       //   mreqs = mreqs - 'This
          for (i <- 0 until typ.size)
          mcreqs.get(typ(i)) match {
            case None =>
            case Some(cld) =>
              fieldn = cld._2
              cldM = cld._3
              for ((f, typ1) <- field) {
                cld._2.get(f) match {
                  case None => fieldn
                  case Some(typ2) =>
                    fieldn = fieldn - f
                    cons = EqConstraint(typ1, typ2) +: cons
                }
              }

              for ((m, cldm) <- cld._3) {
                var mparam = cldm._2
                sig.get(m) match {
                  case None =>
                    cldM = cldM
                  case Some(mSig) =>
                    cons = EqConstraint(cldm._1, mSig._1) +: cons // unify the return type
                    cons = EqConstraint(cldm._3, mSig._3) +: cons // unify the body
                  var param = mSig._2
                    if (param.size == mparam.size) {
                      for ((x, xC) <- param) {
                        // unify equaly-named parameters

                        for ((p, tParam) <- mparam) {
                          mparam = mparam - p
                          cons = EqConstraint(xC, tParam) +: cons
                        }
                      }
                    }
                    if (mparam.isEmpty) cldM = cldM - m
                    else cldM = cldM - m + (m ->(mSig._1, mparam, mSig._3))
                }
              }
              if (fieldn.isEmpty && cldM.isEmpty)
                creq = creq - typ(i)
              else
                creq = creq - typ(i) ++ Map(typ(i) ->(cld._1, fieldn, cldM))

          }
      }


      //restReq = mreqs


      mcreqs.get(c) match {
        case None =>
        case Some(cld) =>
          fieldn = cld._2
          cldM = cld._3
          for ((f, typ1) <- field) {
            cld._2.get(f) match {
              case None => fieldn
              case Some(typ2) =>
                fieldn = fieldn - f
                cons = EqConstraint(typ1, typ2) +: cons
            }
          }

          for ((m, cldm) <- cld._3) {
            var mparam = cldm._2
            sig.get(m) match {
              case None =>
                cldM = cldM
              case Some(mSig) =>
                cons = EqConstraint(cldm._1, mSig._1) +: cons // unify the return type
                cons = EqConstraint(cldm._3, mSig._3) +: cons // unify the body
              var param = mSig._2

                if (param.size == mparam.size) { // unify equaly-named parameters
                for ((x, xC) <- param) {
                  for ((p, tParam) <- mparam) {
                    mparam = mparam - p
                    cons = EqConstraint(xC, tParam) +: cons
                  }
                }
                }
                else cons =  EqConstraint(TNum, TString) +: cons

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

      ( c, mreqs, creq, cons ++ mcons ++ mCcons)

    case ProgramM =>
      val (tm, req1, creqs1, _) = e.kids(0).typ
      val (tmink, req2, creqs2,  _) = e.kids(1).typ

      val (mcons, mreqs) = mergeReqMaps(req1, req2)
      val (mCons1, mCreqs1) = mergeCReqMaps(creqs1, creqs2)
      //val (mCons2, mCreqs2) = mergeCReqMaps(sig1, sig2)
      val (mCons, mCreqs) = mergeCReqMaps(mCreqs1)//, mCreqs2)

      var fld = Map[Symbol,Type]()
      var superT : Type = null
      var cldM = Map[Symbol, (Type, Map[Symbol, Type], Type)]()

      var cCreqs = mCreqs1
      var restReqs = mreqs
      var crq = mCreqs1
      var cons = Seq[Constraint]()

      mCreqs1.get(tm) match { //mCreqs2.get(m)
        case Some(sig) =>
          mCreqs1.get(tm) match {
            case Some(cld) =>
              cldM = cld._3
              for ((m,(t, params, bod)) <- sig._3) {
                if (cld._3.keySet.exists(_ == m)) {
                  for ((x, xC) <- params) {
                    mreqs.get(x) match {
                      case Some(treq) =>
                        restReqs = restReqs - x
                        cons = EqConstraint(xC, treq) +: cons
                    }

                  }
                  cldM = cldM - m
                }
              }}}

      val cld: ClassDecl = (superT, fld, cldM)
      if (fld.isEmpty && cldM.isEmpty) cCreqs = cCreqs - tm
      for ((c,cldC) <- cCreqs) {
        if (cldC._2.isEmpty && cldC._3.isEmpty) cCreqs = cCreqs - c
      }

      (tm, mreqs,cCreqs, mcons ++ mCons ++ mCons1 )  //(....., mCreqs2, ... ++ mCons2)

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
          mcons = EqConstraint(r1, r2) +: mcons
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
      wasF.get(f) match {
        case None => cldf += (f -> typ)
        case Some(typ1) => mcons = EqConstraint(typ1, typ) +: mcons
      }
    for ((m, mbody) <- cld2._3)
      wasM.get(m) match {
        case None => cldm += (m -> mbody) // mdoby = return type + list of parameters
        case Some(mbody1) =>
          mcons = EqConstraint(mbody1._1, mbody._1) +: EqConstraint(mbody1._3, mbody._3) +: mcons
          var params = mbody1._2
          for ((p, t) <- mbody._2)
            mbody1._2.get(p) match {
              case None => params += (p -> t)
              case Some(t2) => mcons = EqConstraint(t, t2) +: mcons
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
