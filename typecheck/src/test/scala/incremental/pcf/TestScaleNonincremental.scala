package incremental.pcf

import benchmark.ExpGenerator
import constraints.equality
import constraints.equality.ConstraintSystem
import constraints.equality.impl.{SolveContinuousSubstThreshold, SolveContinuousSubst, SolveContinuously, SolveEnd}
import incremental.Node.Node
import incremental.Node._
import incremental._
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import ExpGenerator._

/**
 * Created by seba on 14/11/14.
 */
class TestScaleNonInc[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
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
    Util.log(f"Finalize time\t\t${checker.finalizeTime}%.3fms")
  }

  def typecheckTest(desc: String, e: => Node)(expected: equality.Type): Unit =
    test(s"$classdesc: Type check $desc", SlowTest) {
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

  val range = Set(5, 10)
  scaleTests(range, Add, constantLeaveMaker(Num(1)), leaveDesc="1 .. 1")(_=>TNum)
  scaleTests(range, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)), leaveDesc="1 .. n")(_=>TNum)
  scaleTests(range, Add, constantLeaveMaker(Var('x)), leaveDesc="x .. x", wrap = (h, e) => Abs('x, e))(_=>TFun(TNum, TNum))
  scaleTests(range, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i"))), leaveDesc="x1 .. xn", wrap = (h, e) => Abs(usedVars(h), e))(h => makeFunType(h, TNum, ()=>TNum, TFun))
}

class TestDUSolveEndNonInc extends TestScaleNonInc("DUSolveEnd", new DUCheckerFactory(SolveEnd))
class TestDUSolveContniuouslyNonInc extends TestScaleNonInc("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndNonInc extends TestScaleNonInc("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuouslyNonInc extends TestScaleNonInc("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
class TestBUSolveContinuousSubstNonInc extends TestScaleNonInc("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuousSubstThresholdNonInc extends TestScaleNonInc("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermNonInc extends TestScaleNonInc("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
