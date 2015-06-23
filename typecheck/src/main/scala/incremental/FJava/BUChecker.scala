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

        type Methods = Map[Symbol, (Type, Map[Symbol, Type])]

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
              for ((tc, cld) <- creqs) {
                val fld = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](cld._2, p => p._2)
                var meth = Map[Symbol, (Type, Map[Symbol, Type])]()
                for ((sm, body) <- cld._3) {
                  val m = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](body._2, p => p._2)
                  meth = meth ++ Map(sm ->(body._1, m))
                }
                creqs2 += (tc ->(cld._1, fld, meth))
              } //change the merging of the class declaration
              //val fld = cld._2
              //val creqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)//does not work unification in the class declaration inside
              e.typ = (cs applyPartialSolution t, reqs2, creqs, cs.propagate)
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

          case Num => (TNum, Map(), Map(), Seq())
          case op if op == Add || op == Mul =>
            val (t1, reqs1, creqs1, _) = e.kids(0).typ
            val (t2, reqs2, creqs2, _) = e.kids(1).typ

            val lcons = EqConstraint(TNum, t1)
            val rcons = EqConstraint(TNum, t2)
            val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
            val (mcCons, mCreqs) = mergeCReqMaps(creqs1, creqs2)

          //  val (resCons, resReqs) = mergeCReqMaps(sReqs, mCreqs)

            (TNum, mreqs, mCreqs, mcons :+ lcons :+ rcons)

          case Var =>
            val x = e.lits(0).asInstanceOf[Symbol]
            val X = freshCName()
            val cld: ClassDecl = (null, Map(), Map())
            (X, Map(x -> X), Map(), Seq()) // Map(X -> cld), needed at some examples

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
            val cld: ClassDecl = (null, Map(), Map(m ->(C, param)))

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
              var fi = 'b
              if (getX(ti, subreqs) == 'a) {
                fi = freshField().x
              }
              else {
                fi = getX(ti, subreqs)
              }
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
            var cldM = Map[Symbol, (Type, Map[Symbol, Type])]()

            var sparam = params.toMap

            var sig = Signature(null, retT, m, sparam, e0)

            var cCreqs = creqs
          for ((c, cldD) <- creqs){
                cldD._3.get(m) match {
                  case None => sig = sig
                  case Some(cldM1) =>
                    var param = cldM1._2
                    for (i <- 0 until(params.size)) {
                      val x = params(i)._1
                      val xC = params(i)._2
                      reqs.get(x) match {
                        case None => param = param
                        case Some(treq) =>
                          restReqs = restReqs
                          cons = EqConstraint(xC, treq) +: cons
                          //param. = param - x
                      }
                    }
                    if (param.isEmpty) cldM = cldD._3 - m
                    else cldM = cldD._3 - m +  (m -> (retT, param))

                    sig = Signature(c, retT,m, params.toMap, e0)
                    if (fld.isEmpty && cldM.isEmpty) cCreqs = cCreqs - c
                }
            }
            for ((c,cldC) <- creqs) {
              if (cldC._2.isEmpty && cldC._3.isEmpty) cCreqs = cCreqs - c
            }

            (sig, restReqs, cCreqs, cons)


          case ClassDec =>
            val c = e.lits(0).asInstanceOf[CName]
            val sup = e.lits(1).asInstanceOf[CName]
            val fields = e.lits(2).asInstanceOf[Seq[(Symbol, Type)]]
            val methods = e.lits(3).asInstanceOf[Seq[(Symbol, (Type, Map[Symbol, Type]))]]
            val field = fields.toMap
            val method = methods.toMap

            val cld : ClassDecl = (sup,field , method)

            ( c, Map(), Map(), Seq())

          case ProgramM =>
            val (tm, req1, creqs1, _) = e.kids(0).typ
            val (tmink, req2, creqs2,  _) = e.kids(1).typ

            val (mcons, mreqs) = mergeReqMaps(req1, req2)
            val (mCons1, mCreqs1) = mergeCReqMaps(creqs1, creqs2)
            //val (mCons2, mCreqs2) = mergeCReqMaps(sig1, sig2)
            val (mCons, mCreqs) = mergeCReqMaps(mCreqs1)//, mCreqs2)

            var fld = Map[Symbol,Type]()
            var superT : Type = null
            var cldM = Map[Symbol, (Type, Map[Symbol, Type])]()

            var cCreqs = mCreqs1
            var restReqs = mreqs
            var crq = mCreqs1
            var cons = Seq[Constraint]()

            mCreqs1.get(tm) match { //mCreqs2.get(m)
              case Some(sig) =>
                mCreqs1.get(tm) match {
                  case Some(cld) =>
                    cldM = cld._3
                    for ((m,(t, params)) <- sig._3) {
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
                mcons = EqConstraint(mbody1._1, mbody._1) +: mcons
                var params = mbody1._2
                for ((p, t) <- mbody._2)
                  mbody1._2.get(p) match {
                    case None => params += (p -> t)
                    case Some(t2) => mcons = EqConstraint(t, t2) +: mcons
                  }
                cldm = Map(m ->(mbody1._1, params))
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
