package incremental.FJava

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

        type ClassDecl = (Type, Map[Symbol, Type], Map[Symbol, (Type, Map[Symbol, Type])]) //(Type, Fields, Methods)

        type Reqs = Map[Symbol, Type]

        type CReqs = Map[Symbol, ClassDecl]

        def Subtype(C: Symbol, D: Type): CReqs = {
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
              e.typ = (cs applyPartialSolution t, reqs2, creqs, cs.propagate)
              val creqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](reqs, p => p._2)//does not work unification in the class declaration inside
              e.typ = (cs applyPartialSolution t, reqs2, creqs, cs.propagate)
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

          case Num => (TNum, Map(), Map(), Seq())
          case op if op == Add || op == Mul =>
            val (t1, reqs1, creqs1, _) = e.kids(0).typ
            val (t2, reqs2, creqs2, _) = e.kids(1).typ

            val lcons = EqConstraint(TNum, t1)
            val rcons = EqConstraint(TNum, t2)
            val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)
            val (mcCons, mCreqs) = mergeCReqMaps(creqs1, creqs2)

            (TNum, mreqs, mCreqs, mcons :+ lcons :+ rcons)

          case Var =>
            val x = e.lits(0).asInstanceOf[Symbol]
            val X = freshCName()
            val cld: ClassDecl = (null, Map(), Map())
            (X, Map(x -> X), Map(), Seq())// Map(X -> cld), needed at some examples

          case Fields =>
            val f = e.lits(0).asInstanceOf[Symbol] //symbol
          val (t, reqs, creqs, _) = e.kids(0).typ //subsol
          val U = freshCName()
            val ct: ClassDecl = (null, Map(f -> U), Map())

            val (cons, mcreqs) = mergeCReqMaps(creqs, Map(getX(t,reqs) -> ct))
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
              var xi = 'b
              if (getX(ti, reqs0) == 'a) { //subreqs
                xi = freshParam().x
              }
              else {
                xi = getX(ti, reqs0)
              }

              reqss = reqss :+ subreqs
              creqss = creqss :+ subcreqs
              param += (xi -> ti)
            }
            val cld: ClassDecl = (null, Map(), Map(m ->(C, param)))

            val (mcons, mreqs) = mergeReqMaps(reqss)
            val (cCons, creqs) = mergeCReqMaps(creqss)
            val (mcCons, mcreqs) = mergeCReqMaps(creqs, Map(getX(t0,reqs0) -> cld))

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
            val (mcCons, mcreqs) = mergeCReqMaps(creqs, Map(c.x -> cld))
            (c, mreqs, mcreqs, mcons ++ cCons ++ mcCons)


          case DCast =>
            val (t, reqs, creqs, _) = e.kids(0).typ
            val c = e.lits(0).asInstanceOf[CName]
            (c, reqs, creqs ++ Subtype(c.x, c), Seq((NotEqConstraint(t, c))))

          case UCast =>
            val c = e.lits(0).asInstanceOf[CName]
            val (t, reqs, creqs, _) = e.kids(0).typ

            (c, reqs, creqs ++ Subtype(getX(t,reqs), c), Seq())

          case SCast =>
            val c = e.lits(0).asInstanceOf[CName]
            val (t, reqs, creqs, _) = e.kids(0).typ
            (t, reqs, creqs, Seq())

          case MethodDec =>
            val (e0, reqs, creqs, _) = e.kids(0).typ
            val C = e.lits(0).asInstanceOf[CName]
            val retT = e.lits(1).asInstanceOf[CName]
            val m = e.lits(2).asInstanceOf[Symbol]
            var param = Map[Symbol, Type]()
            var restReqs = reqs
            var cons = Seq[Constraint]()
            val params = e.lits(3).asInstanceOf[Seq[(Symbol, Type)]]
            var fld = Map[Symbol,Type]()
            var superT : Type = null
            var cldM = Map[Symbol, (Type, Map[Symbol, Type])]()

            creqs.get(C.x) match {
              case None =>
              case Some(cldD) =>
                superT = cldD._1
                fld = cldD._2
                cldM = cldD._3
                cldD._3.get(m) match {
                  case None => cldM = cldD._3
                  case Some(m1) =>
                    param = m1._2
                    cons = EqConstraint(m1._1, retT) +: cons
                    for (i <- 0 until params.size) {
                      val x = params(i)._1
                      val xC = params(i)._2
                      reqs.get(x) match {
                        case Some(treq) =>
                          restReqs = restReqs - x
                          cons = EqConstraint(xC, treq) +: cons
                      }

                      param.get(x) match {
                        case Some(t1) => cons = EqConstraint(xC, t1) +: cons
                          param = param - x
                      }

                    }
                }
                if (param.isEmpty) {
                  cldM = cldM - m
                }
                else cldM = cldM
            }

            val cld: ClassDecl = (superT, fld, cldM)


          //  val (mcons, mcreqs) = mergeCReqMaps(creqs, Map(C -> cld))

            (C, restReqs, Map(C.x -> cld), cons)

          //    case TClass =>
          //      val (t, reqs, creqs, _) = e.kids(0).typ
          //      (t, reqs, creqs, Seq())
          //

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
