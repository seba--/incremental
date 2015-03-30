package incremental.FJava

import incremental.FJava
import incremental.ClassT._
import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type
import incremental._

import scala.concurrent.SyncChannel

//import incremental.systemf.{Var, TNum, Num, ConstraintOps}


/**
 * Created by lirakuci on 3/2/15.
 */
class Class(val name: Name, superClass: Name, val fields: List[Fields], val m: List[Methods])

class Fields(fname: Symbol, val fType: Type)

class Methods(val mtype: Type, mname: Name, margs: List[Parameter])

class ClassDecl(cName: Symbol, cSuper: Symbol, cFld: List[Fields], cMethods: List[Methods])

class BottomUpChecker extends TypeChecker[Type] {
  val constraint = new ConstraintOps

  import constraint._

  var preparationTime = 0.0
  var typecheckTime = 0.0



  def constraintCount = constraint.constraintCount

  def mergeReqsTime: Double = constraint.mergeReqsTime

  def cmergeReqsTime = constraint.cmergeReqsTime

  def constraintSolveTime = constraint.constraintSolveTime

  def mergeSolutionTime = constraint.mergeSolutionTime

  type Reqs = Map[Symbol, Type]

  type CReqs = Map[Symbol,ClassDecl]

  val ct = new ClassDecl(null, null, List(), List())

  type Result = (Type, Reqs, CReqs, Solution)


  def Subtype(C: Symbol, D : Symbol) : CReqs = {
    val cld = new ClassDecl(C, D, List(), List())
    Map(C -> cld)
  }

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

      case Var =>
        val x = e.lits(0).asInstanceOf[Symbol]
        val X = freshUVar()
        (X, Map(x -> X), Map(), emptySol)

      case Field if (e.lits(0).isInstanceOf[Symbol]) =>
        val f = e.lits(0).asInstanceOf[Symbol]
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        //val e0 = t.asInstanceOf[Symbol]
        val U = freshUVar()
        val fld = new Fields(f,U)
        val ct = new ClassDecl(f, null, List(fld), List())
        (U, reqs, creqs + (f -> ct), subsol)

      case New =>
        val c = e.lits(0).asInstanceOf[Symbol]
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        val U = freshUVar()
        val d = U.x
        (t, reqs, creqs ++ Subtype(c, d), subsol)


    }
  }

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}