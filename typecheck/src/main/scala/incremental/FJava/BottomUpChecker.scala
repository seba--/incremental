package incremental.FJava

import incremental.FJava
import incremental.ClassT._
import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type
import incremental._
//import incremental.systemf.{Var, TNum, Num, ConstraintOps}


/**
 * Created by lirakuci on 3/2/15.
 */
class Class(val name: Name, superClass: Name, val fields: List[Fields], val m: List[Methods])

class Fields(fname: Name, val fType: Type)

class Methods(val mtype: Type, mname: Name, margs: List[Parameter])

class ClassDecl(cName: Type, cSuper: Type, cFld: List[Fields], cMethods: List[Methods]) {

  //val cFields = cFld
}

class BottomUpChecker extends TypeChecker[Type] {
  val constraint = new ConstraintOps

  import constraint._

//def  ClassDecl(cName: Name, cSuper: Name, cFields : List[Symbol => String], cMethods : List[Method])


 // def  CT(name: Name) : ClassDecl = name match {
   // case name => ClassDecl(name, super,  List[Field], List[Method] )
  //}
  //type ClassDecl

  var preparationTime = 0.0
  var typecheckTime = 0.0



  def constraintCount = constraint.constraintCount

  def mergeReqsTime: Double = constraint.mergeReqsTime

  def cmergeReqsTime = constraint.cmergeReqsTime

  def constraintSolveTime = constraint.constraintSolveTime

  def mergeSolutionTime = constraint.mergeSolutionTime

  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Type,ClassDecl]

  val ct = new ClassDecl(null, null, List(), List())
//cls.cFields
  type Result = (Type, Reqs, CReqs, Solution)

//type cReqs Map[Name, String ]

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

    //    val (uninitialized, ptime) = Util.timed {root.uninitialized}
    //    preparationTime += ptime

    val (res, ctime) = Util.timed {
      root.visitUninitialized { e =>
        e.typ = typecheckStep(e)
        true
      }

      val (t_, reqs, creqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!creqs.isEmpty)
        Right(s"Unresolved type variables requirements $creqs, type variables $t, tunres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
    typecheckTime += ctime
    res
  }
    def typecheckStep(e: Exp_[Result]): Result = e.kind match{

      case Sub =>
        val t1 = e.lits(0).asInstanceOf[Type]
        val t2 = e.lits(0).asInstanceOf[Type]
        val ct = new ClassDecl(t1, t2, List(), List())
        (null, Map(),Map(t1 -> ct), emptySol)

//      case Fields =>
//        val c = e.lits(0).asInstanceOf[Symbol]
//        val u = e.lits(1).asInstanceOf[Symbol]
//        val (t1, reqs1, creqs1, subsol1) = e.kids(0).typ
//        val (t2, reqs2, creqs2, subsol2) = e.kids(1).typ
//        val ct = new ClassDecl(t1, t2, List(), List())
//        creqs1.get(t1) match {
//          case None => (t1, reqs1 ++ reqs2, creqs2 + (t1 -> ct),  subsol1 +++ subsol2)
//          case Some(ctreq) =>
//            (t1, reqs1 ++ reqs2, creqs1 ++ creqs2 + (t1 -> ct), subsol1 +++ subsol2)
//        }

      case Var =>
        val x = e.lits(0).asInstanceOf[Symbol]
        val X = freshUVar()
        (X, Map(x -> X), Map(), emptySol)

      case Field if (e.lits(0).isInstanceOf[Symbol]) =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        val f = e.lits(0).asInstanceOf[Name]
        val U = freshUVar()

        val fld = new Fields(f,U)

        val ct = new ClassDecl(t, null, List(fld), List())
        (t, reqs, creqs + (t -> ct), subsol)


    }
  }

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}