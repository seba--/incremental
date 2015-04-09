package incremental.pcf_graph

import benchmark.ExpGenerator
import incremental.Node.Exp
import incremental.Node._
import incremental._
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import ExpGenerator._

/**
 * Created by seba on 14/11/14.
 */
class TestScaleNonincremental(classdesc: String, checkerFactory: TypeCheckerFactory[Type]) extends FunSuite with BeforeAndAfterEach {
  var checker: TypeChecker[Type] = _

  override def beforeEach: Unit = {
    checker = checkerFactory.makeChecker
  }

  override def afterEach: Unit = {
    Util.log(f"Preparation time\t${checker.preparationTime}%.3fms")
    Util.log(f"Type-check time\t\t${checker.typecheckTime}%.3fms")
    Util.log(f"Constraint count\t${checker.constraintCount}")
    Util.log(f"Cons. solve time\t${checker.constraintSolveTime}%.3fms")
    Util.log(f"Merge reqs time\t\t${checker.mergeReqsTime}%.3fms")
    Util.log(f"Merge sol time\t\t${checker.mergeSolutionTime}%.3fms")
  }

  def typecheckTest(desc: String, e: => Exp)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
    }

  def scaleTests(heights: Set[Int], kind: NodeKind, leaveMaker: LeaveMaker, sharing: Boolean = false, leaveDesc: String = "", wrap : (Int,Exp) => Exp = (_,e) => e)(expected: Int => Type) =
    for (h <- heights)
      scaleTest(h, kind, leaveMaker, sharing, leaveDesc, wrap)(expected)

  def scaleTest(height: Int, kind: NodeKind, leaveMaker: LeaveMaker, sharing: Boolean = false, leaveDesc: String = "", wrap : (Int,Exp) => Exp = (_,e) => e)(expected: Int => Type) =
    typecheckTest(
      s"${if(sharing) "shared" else "non-shared"} $kind-tree(h=$height)${if(leaveDesc.isEmpty)"" else " with leaves " + leaveDesc}",
      wrap(height, makeBinTree(height, kind, leaveMaker, sharing)))(expected(height))

  scaleTests(Set(5, 10, 15, 20), Add, constantLeaveMaker(Num(1)), leaveDesc="1 .. 1")(_=>TNum)
  scaleTests(Set(5, 10, 15, 20), Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)), leaveDesc="1 .. n")(_=>TNum)
  scaleTests(Set(5, 10, 15, 20), Add, constantLeaveMaker(Var('x)), leaveDesc="x .. x", wrap = (h, e) => Abs('x, e))(_=>TFun(TNum, TNum))
  scaleTests(Set(5, 10, 15, 18), Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i"))), leaveDesc="x1 .. xn", wrap = (h, e) => Abs(usedVars(h), e))(h => makeBinType(h, TNum, ()=>TNum, TFun(_, _)))
}
class TestDownUpScaleNonincremental extends TestScaleNonincremental("DownUp", DownUpCheckerFactory)
//class TestDownUpSolveEndScaleNonincremental extends TestScaleNonincremental("DownUpSolveEnd", DownUpSolveEndCheckerFactory)
class TestBottomUpScaleNonincremental extends TestScaleNonincremental("BottomUp", BottomUpEagerSubstCheckerFactory)
//class TestBottomUpSolveEndScaleNonincremental extends TestScaleNonincremental("BottomUpSolveEnd", BottomUpSolveEndCheckerFactory)
//class TestBottomUpEarlyTermScaleNonincremental extends TestScaleNonincremental("BottomUpEarlyTerm", BottomUpEarlyTermCheckerFactory)
