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

class Methods(val mtype: Symbol, mparam: List[Type], mreturnT : Type)
{
  val param = this.mparam
}

class ClassDecl(cName: Type , cSuper: Type, cFld: List[Fields], cMethods: List[Methods])
{
  val c = this.cName
  val s = this.cSuper
  val f = this.cFld
  val m = this.cMethods
}

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


  def Subtype(C: Type, D: Type): CReqs = {
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
      val U = freshCName()
      val fld = new Fields(f, U)
      val ct = new ClassDecl(t, null, List(fld), List())
      (U, reqs, creqs + (t -> ct), subsol)

    case Invk =>
      if (e.kids.seq.size == 1 ) {
        val m = e.lits(0).asInstanceOf[Symbol]
        val (e0, reqs1, creqs1, subsol1) = e.kids(0).typ
        val C = freshCName()
        val method = new Methods(m, List(), C)
        val cld = new ClassDecl(e0, null, List(), List(method))
        (C, reqs1, creqs1 + (e0 -> cld), subsol1)

      }
      else {
        val m = e.lits(0).asInstanceOf[Symbol]
        val C = freshCName()
        var mcons = Seq[Constraint]()
        var mreqs: Reqs = Map()
        var mcreqs: CReqs = Map()
        var param =  List[Type]()
        var method = new Methods(m, param, C)
        var msol = emptySol
        val (e0, reqs1, creqs1, subsol1) = e.kids(0).typ
        val subs = for (sub <- e.kids.seq drop(0)) yield {
          val (ei, subreqs, subcreqs, subsol) = sub.typ
          msol = msol +++ subsol
          val (cons, reqs) = mergeReqMaps(subreqs, subreqs)
          val di = freshUVar()
          subcreqs ++ Subtype(ei,di)
         val (cCons, creqs ) = mergeCReqMaps(subcreqs, subcreqs)
          mcons = mcons ++ cons ++ cCons
          mreqs = reqs
          mcreqs = subcreqs
          param ++= List(ei)
        }
        val sol = solve(mcons)
        val cld = new ClassDecl(e0, null, List(), List(method))
        (C, mreqs.mapValues(_.subst(sol.substitution)),mcreqs ++ creqs1 , msol <++ sol ++ subsol1)

      }

    case New =>
      val c = e.lits(0).asInstanceOf[CName]
      if (e.kids.seq == Seq()) {
        val cld = new ClassDecl(c, null, List(), List())
        (c, Map(), Map(c -> cld), emptySol)
      }
      else {
        val U = freshCName()
        var mcons = Seq[Constraint]()
        var mreqs: Reqs = Map()
        var mcreqs: CReqs = Map()
        var field = List[Fields]()
        var msol = emptySol
        val subs = for (sub <- e.kids.seq) yield {
          val (ei, subreqs, subcreqs, subsol) = sub.typ
          msol = msol +++ subsol
          val (cons, reqs) = mergeReqMaps(subreqs, subreqs)
          val di = freshUVar()
          subcreqs ++ Subtype(ei,di)
          val (cCons, creqs ) = mergeCReqMaps(subcreqs, subcreqs)
          mcons = mcons ++ cons ++ cCons
          mreqs = reqs
          mcreqs = subcreqs
          val fld = new Fields(di.x, di)
          field ++= List(fld)
        }
        val sol = solve(mcons)
        val cld = new ClassDecl(c, null, field, List())
        (c, mreqs.mapValues(_.subst(sol.substitution)),mcreqs , msol <++ sol)

      }

     // val (t, reqs, creqs, subsol) = e.kids(0).typ
     // val f = U.x
     // val fld = new Fields(f, U)
     // val cld = new ClassDecl(c, null, List(fld), List())
     // (c, reqs, creqs + (c -> cld) ++ Subtype(t, U), subsol)
     // }

    case DCast =>
      val (t, reqs, creqs, subsol) = e.kids(0).typ
      val c = e.lits(0).asInstanceOf[CName]
      val sol = solve(NotEqConstraint(t, c))
      (c, reqs, creqs ++ Subtype(c, c), subsol ++ sol)

    case UCast =>
      val c = e.lits(0).asInstanceOf[CName]
      val (t, reqs, creqs, subsol) = e.kids(0).typ

      (c, reqs, creqs ++ Subtype(t, c), subsol)

    case SCast =>
      val (t, reqs, creqs, subsol) = e.kids(0).typ
      (t, reqs, creqs, subsol)

    case Method =>

      val (e0, reqs, creqs, subsol) = e.kids(0).typ
      val C0 = e.lits(0).asInstanceOf[Type]
      val m = e.lits(1).asInstanceOf[Symbol]
      val x = e.lits(2).asInstanceOf[Symbol]
      val C = e.lits(3).asInstanceOf[Type]

      reqs.get(x) match {
        case None =>
          val Ci = if (e.lits == 5) e.lits(4).asInstanceOf[Type] else freshUVar()
          val method = new Methods(m, List(Ci), C)
          val cld = new ClassDecl(C, null, List(), List(method))
          (C0, reqs, creqs ++ Subtype(e0, C0) + (C -> cld), subsol)
        case Some(treq) =>
          val otherReqs = reqs - x
          if (e.lits.size == 5) {
            val Ci = e.lits(4).asInstanceOf[Type]
            val sol = solve(EqConstraint(Ci, treq))
            val method = new Methods(m, List(Ci), C)
            val cld = new ClassDecl(C, null, List(), List(method))
            (C0, otherReqs.mapValues(_.subst(sol.substitution)), creqs ++ Subtype(e0, C0) + (C -> cld), subsol <++ sol)
          }
          else
            (C0, otherReqs, creqs ++ Subtype(e0, C0), subsol)
      }

    case TClass =>
      val (t, reqs, creqs, subsol) = e.kids(0).typ
      (t, reqs, creqs, subsol)


  }
}


object BottomUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new BottomUpChecker
}

