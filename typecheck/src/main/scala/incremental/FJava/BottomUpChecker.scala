package incremental.FJava

import incremental.ClassT._
import incremental.ConstraintOps._
import incremental.Node.Node
import incremental._
import incremental.Type



/**
 * Created by lirakuci on 3/2/15.
 */
class Class(val name: Name, superClass: Name, val fields: List[Fields], val m: List[Methods])

class Fields(fname: Symbol, val fType: Type)

class Methods(val mtype: Type, margs: List[Type], mreturnT : Type)

class ClassDecl(cName: Type , cSuper: Type, cFld: List[Fields], cMethods: List[Methods])

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

  type CReqs = Map[Type, ClassDecl]

  val ct = new ClassDecl(null, null, List(), List())

  type Result = (Type, Reqs, CReqs, Solution)


  def Subtype(C: Type, D : Type) : CReqs = {
    val cld = new ClassDecl(C, D, List(), List())
    Map(C -> cld)
  }

  def typecheck(e: Node): Either[Type, TError] = {
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
    def typecheckStep(e: Node_[Result]): Result = e.kind match {

      case Var =>
        val x = e.lits(0).asInstanceOf[Symbol]
        val X = freshUVar()
        (X, Map(x -> X), Map(), emptySol)

      case Field =>
        val f = e.lits(0).asInstanceOf[Symbol]
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        //t.unify(e0)
        val U = freshUVar()
        val fld = new Fields(f, U)
        val ct = new ClassDecl(t, null, List(fld), List())
        (U, reqs, creqs + (t -> ct), subsol)
      //have to look again and it should be t, instead of c

      case New =>
        val c = e.lits(0).asInstanceOf[CName]
        if (e.kids.seq == Seq()) {
          val cld = new ClassDecl(c, null, List(), List())
          (c, Map(), Map(c -> cld), emptySol)
        }
        else {
          val (t, reqs, creqs, subsol) = e.kids(0).typ
          val U = freshUVar()
          val f = U.x
          val fld = new Fields(f, U)
          val cld = new ClassDecl(c, null, List(fld), List())
          (c, reqs, creqs + (c -> cld) ++ Subtype(t, U), subsol)
        }

      case DCast =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        val c = e.lits(0).asInstanceOf[CName]
        val sol = solve(NotEqConstraint(t,c))
        (c, reqs, creqs ++ Subtype(c, c), subsol ++ sol)

      case UCast =>
        val c = e.lits(0).asInstanceOf[CName]
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        (c, reqs, creqs ++ Subtype(t,c), subsol)

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
        val cld = new ClassDecl(e0.asInstanceOf[CName], null, List(), List(method))
        (c, reqs1 ++ reqs2 , creqs1 ++ creqs2 ++ Subtype(ei,d), subsol1 ++ subsol2)

      case TClass =>
        val (t, reqs, creqs, subsol) = e.kids(0).typ
        (t, reqs, creqs, subsol)

    }
  }

object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}

