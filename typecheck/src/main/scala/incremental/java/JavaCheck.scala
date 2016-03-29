package incremental.java

import constraints.CVar
import constraints.javacons._
import incremental.java.syntax._
import incremental.Node_

/**
 * Created by qwert on 25.08.15.
 */

class JavaContext extends incremental.Context[Constraint]

object JavaCheck {
  trait CheckRes
  case object ClassOk extends CheckRes
  case object MethodOk extends CheckRes
  case object StmOk extends CheckRes
  case class ExprType(t: Type) extends CheckRes

  type VReqs = Map[Symbol, Type]
  type CReqs = Seq[Any] // TODO: find suitable type for class requirements

  type Result = (CheckRes, VReqs, CReqs)
  type Kid = Node_[Constraint, _, Result]

  val emptyCReqs : CReqs = Seq()
  val emptyVReqs : VReqs = Map()
  val emptyCons : Seq[Constraint] = Seq()

  val TString : Type = ClassOrInterfaceType(TypeNameExt(PackageOrTypeNameExt(PackageOrTypeNameT("java"), "lang"), "String"), None)

  val primTypes: Seq[Type] = Seq(TByte(), TShort(), TInt(), TLong(), TChar(), TFloat(), TDouble(), TBoolean())
  val numTypes: Seq[Type] = Seq(TByte(), TShort(), TInt(), TLong(), TChar(), TFloat(), TDouble())
  val integralTypes: Seq[Type] = Seq(TByte(), TShort(), TInt(), TLong(), TChar())
  val floatTypes: Seq[Type] = Seq(TFloat(), TDouble())
  val numericOpsTypes: Seq[Type] = Seq(TInt(), TLong(), TFloat(), TDouble())

  val gen = new Gen

  def freshUVar()  = UVar(gen.freshSymbol[Type]("T"))

  private val init: (Seq[Constraint], VReqs) = (emptyCons, emptyVReqs)

  def mergeVReqs(req: VReqs, reqs: VReqs*): (Seq[Constraint], VReqs) = mergeVReqs(req +: reqs)

  def mergeVReqs(reqs: Seq[VReqs]): (Seq[Constraint], VReqs) = reqs.foldLeft[(Seq[Constraint], VReqs)](init)(_mergeVReqs)

  private def _mergeVReqs(was: (Seq[Constraint], VReqs), newReqs: VReqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs

    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) => mcons = Equality(r1, r2) +: mcons
      }

    (mcons, mreqs)
  }

  def mergeCReqs(reqs1: CReqs, reqs2: CReqs): CReqs = reqs1 ++ reqs2

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
