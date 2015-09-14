package incremental.Java

import constraints.CVar
import constraints.javacons._
import incremental.Java.syntax._
import incremental.Node_

/**
 * Created by qwert on 25.08.15.
 */

/* TODO make Constraint system imports precise (javacons instead of general constraint.*)
 *
 */
object JavaCheck {
  trait CheckRes
  case object ClassOk extends CheckRes
  case object MethodOk extends CheckRes
  case class StmType(t: Type) extends CheckRes
  case class ExprType(t: Type) extends CheckRes

  //type CS <: ConstraintSystem[CS]

  type VReqs = Map[Symbol, Type]
  type CReqs = Map[Symbol, Type] // TODO: find suitable type for class requirements

  type Result = (CheckRes, VReqs, CReqs, CS)
  type StepResult = (CheckRes, VReqs, CReqs, Seq[Constraint])
  type Kid = Node_[StepResult]

  val emptyCReqs : CReqs = Map()
  val emptyVReqs : VReqs = Map()
  val emptyCons : Seq[Constraint] = Seq()

  val TString : Type = ClassOrInterfaceType(TypeNameExt(PackageOrTypeNameExt(PackageOrTypeNameT("java"), "lang"), "String"), None)

  val numTypes: Seq[Type] = Seq() // TODO: list numeric types (primitive)

  def freshUVar()  = UVar(CVar(Symbol("foo"))) // TODO: fresh symbol creation?

  def mergeVReqs(reqs1: VReqs, reqs2: VReqs): (Seq[Constraint], VReqs) = {
    var cons = emptyCons
    var reqs = reqs1

    for((x, t2) <- reqs2){
      reqs1.get(x) match {
        case None => reqs += x -> t2
        case Some(t1) => Equality(t1, t2) +: cons
      }
    }

    (cons, reqs)
  }

  //val csf: ConstraintSystemFactory[CS]

  //def checkStep(lits: Seq[Any], kids: Seq[Kid]): StepResult // = (ExprType(TInt()), Map(), Map(), Seq()) // default impl to avoid build errors for non-impl subclasses


  /*def typecheckRec(e: Node_[Result]): Unit = {
    val res@(t, creqs, vreqs, cons) = e.kind.check(e.lits, e.kids.seq)
    val subcs = e.kids.seq.foldLeft(csf.freshConstraintSystem)((cs, res) => cs mergeSubsystem res.typ._4)
    val cs = subcs addNewConstraints cons
    val creqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](creqs, p => p._2)
    val vreqs2 = cs.applyPartialSolutionIt[(Symbol, Type), Map[Symbol, Type], Type](vreqs, p => p._3)
    e.typ = (cs applyPartialSolution t, creqs2, vreqs2, cs.propagate)

  }*/
}
