package incremental.FJava

import incremental.ClassT._
import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Type
import incremental._


/**
 * Created by lirakuci on 3/2/15.
 */
class Class(val name: Name, superClass: Name, val fields: List[Fields], val m: List[Methods])

class Fields(fname: Symbol, val fType: Type)

class Methods(val mtype: Type, margs: List[Type], mreturnT : Type)

class ClassDecl(cName: Symbol , cSuper: Symbol, cFld: List[Fields], cMethods: List[Methods])

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

  type CReqs = Map[Symbol, ClassDecl]

  val ct = new ClassDecl(null, null, List(), List())

  type Result = (Type, Reqs, CReqs, Solution)


  def Subtype(C: Type, D : Type) : CReqs = {
    val cld = new ClassDecl(C.asInstanceOf[UVar].x, D.asInstanceOf[UVar].x, List(), List())
    Map(C.asInstanceOf[UVar].x -> cld)
  }

  def typecheck(e: Exp): Either[Type, TError] = {
    val root = e.withType[Result]

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
    def typecheckStep(e: Exp_[Result]): Result = e.kind match {

      case Var =>
        val x = e.lits(0).asInstanceOf[Symbol]
        val X = freshUVar()
        (X, Map(x -> X), Map(), emptySol)

      case Field =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        val f = e.lits(0).asInstanceOf[CName]
        val U = freshUVar()
        val fld = new Fields(f.x, U)
        val ct = new ClassDecl(f.x, null, List(fld), List())
        (U, reqs, creqs + (f.x -> ct), subsol)
      //have to look again and it should be t, instead of c

      case New =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        val c = e.lits(0).asInstanceOf[CName]
        val U = freshUVar()
        val f = U.x
        val fld = new Fields(f, U)
        val cld = new ClassDecl(c.x, null, List(fld), List())
        (c, reqs, creqs + (c.x -> cld) ++ Subtype(t, U) , subsol)

      case UCast =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        val c = e.lits(0).asInstanceOf[Type]
        val sol = solve(NotEqConstraint(t,c))
        (c, reqs, creqs ++ Subtype(t, c), subsol ++ sol)

      case DCast =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        val c = e.lits(0).asInstanceOf[Type]
        (c, reqs, creqs ++ Subtype(t, c), subsol)
      case SCast =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        (t, reqs, creqs, subsol)


      case Invk =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        (t, reqs, creqs, subsol)

      case Method =>
        val (e0, reqs1, creqs1, subsol1) = e.kids(0).typ
        val (ei, reqs2, creqs2, subsol2) = e.kids(1).typ
        val m = e.lits(0).asInstanceOf[Type]
        val c = freshUVar()
        val d = freshUVar()
        val method = new Methods(m,List(ei),c)
        val cld = new ClassDecl(e0.asInstanceOf[CName].x, null, List(), List(method))
        (c, reqs1 ++ reqs2 , creqs1 ++ creqs2 ++ Subtype(ei,d), subsol1 ++ subsol2)

      case TClass =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        (t, reqs, creqs, subsol)

    }
  }

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}

