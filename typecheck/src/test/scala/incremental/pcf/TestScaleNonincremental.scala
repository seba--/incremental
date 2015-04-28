package incremental.pcf

import benchmark.ExpGenerator
import constraints.equality
import constraints.equality.ConstraintSystem
import constraints.equality.config.{SolveContinuousSubstThreshold, SolveContinuousSubst, SolveContinuously, SolveEnd}
import incremental.Node.Node
import incremental.Node._
import incremental._
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import ExpGenerator._

/**
 * Created by seba on 14/11/14.
 */
class TestScaleNonincremental[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

//  override def beforeEach: Unit = {
//    checker = checkerFactory.makeChecker
//  }

  override def afterEach: Unit = {
    Util.log(f"Preparation time\t${checker.preparationTime}%.3fms")
    Util.log(f"Type-check time\t\t${checker.typecheckTime}%.3fms")
    Util.log(f"Constraint count\t${checker.constraintCount}")
    Util.log(f"Cons. solve time\t${checker.constraintSolveTime}%.3fms")
    Util.log(f"Merge reqs time\t\t${checker.mergeReqsTime}%.3fms")
    Util.log(f"Merge sol time\t\t${checker.mergeSolutionTime}%.3fms")
  }

  def typecheckTest(desc: String, e: => Node)(expected: equality.Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
    }

  def scaleTests(heights: Set[Int], kind: NodeKind, leaveMaker: LeaveMaker, sharing: Boolean = false, leaveDesc: String = "", wrap : (Int,Node) => Node = (_,e) => e)(expected: Int => equality.Type) =
    for (h <- heights)
      scaleTest(h, kind, leaveMaker, sharing, leaveDesc, wrap)(expected)

  def scaleTest(height: Int, kind: NodeKind, leaveMaker: LeaveMaker, sharing: Boolean = false, leaveDesc: String = "", wrap : (Int,Node) => Node = (_,e) => e)(expected: Int => equality.Type) =
    typecheckTest(
      s"${if(sharing) "shared" else "non-shared"} $kind-tree(h=$height)${if(leaveDesc.isEmpty)"" else " with leaves " + leaveDesc}",
      wrap(height, makeBinTree(height, kind, leaveMaker, sharing)))(expected(height))

  scaleTests(Set(5, 10, 15, 20), Add, constantLeaveMaker(Num(1)), leaveDesc="1 .. 1")(_=>TNum)
  scaleTests(Set(5, 10, 15, 20), Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)), leaveDesc="1 .. n")(_=>TNum)
  scaleTests(Set(5, 10, 15, 20), Add, constantLeaveMaker(Var('x)), leaveDesc="x .. x", wrap = (h, e) => Abs('x, e))(_=>TFun(TNum, TNum))
  scaleTests(Set(5, 10, 15, 18), Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i"))), leaveDesc="x1 .. xn", wrap = (h, e) => Abs(usedVars(h), e))(h => makeFunType(h, TNum, ()=>TNum, TFun))
}

//class TestDUSolveEndScalaNoInc extends TestScaleNonincremental("DUSolveEnd", new DUCheckerFactory(SolveEnd))
//class TestDUSolveContniuouslyScalaNoInc extends TestScaleNonincremental("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))
//class TestBUSolveEndScalaNoInc extends TestScaleNonincremental("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyScalaNoInc extends TestScaleNonincremental("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstScalaNoInc extends TestScaleNonincremental("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuousSubstThresholdScalaNoInc extends TestScaleNonincremental("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEarlyTermScaleNonincremental extends TestScaleNonincremental("BottomUpEarlyTerm", BottomUpEarlyTermCheckerFactory)
