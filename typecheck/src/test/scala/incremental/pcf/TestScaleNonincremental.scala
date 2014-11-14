package incremental.pcf

import incremental.Exp.Exp
import incremental.Exp._
import incremental._
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import Generator._

/**
 * Created by seba on 14/11/14.
 */
class TestScaleNonincremental(classdesc: String, checkerFactory: TypeCheckerFactory) extends FunSuite with BeforeAndAfterEach {
  var checker: TypeChecker = _

  override def beforeEach: Unit = {
    checker = checkerFactory.makeChecker
  }

  override def afterEach: Unit = {
    Util.log(f"Preparation time\t${checker.preparationTime}%.3fms")
    Util.log(f"Type-check time\t\t${checker.typecheckTime}%.3fms")
    Util.log(f"Constraint count\t${checker.constraintCount}")
    Util.log(f"Cons. solve time\t${checker.constraintSolveTime}%.3fms")
    Util.log(f"Merge reqs time\t\t${checker.mergeReqsTime}%.3fms")
  }

  def typecheckTest(desc: String, e: => Exp)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
    }

  def scaleTests(heights: Set[Int], kind: ExpKind, leaveMaker: LeaveMaker, sharing: Boolean = false, leaveDesc: String = "")(expected: Type) =
    for (h <- heights)
      scaleTest(h, kind, leaveMaker, sharing, leaveDesc)(expected)

  def scaleTest(height: Int, kind: ExpKind, leaveMaker: LeaveMaker, sharing: Boolean = false, leaveDesc: String = "")(expected: Type) =
    typecheckTest(
      s"${if(sharing) "shared" else "non-shared"} $kind-tree(h=$height)${if(leaveDesc.isEmpty)"" else " with leaves " + leaveDesc}",
      makeBinTree(height, kind, leaveMaker, sharing))(expected)

  scaleTests(Set(5, 10, 15, 20), Add, constantLeaveMaker(Num(1)), leaveDesc="1 .. 1")(TNum)

  def next = () => {

  }
  scaleTests(Set(5, 10, 15, 20), Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)), leaveDesc="1 .. n")(TNum)
}
class TestDownUpScaleNonincremental extends TestScaleNonincremental("DownUp", DownUpCheckerFactory)
class TestBottomUpScaleNonincremental extends TestScaleNonincremental("BottomUp", BottomUpCheckerFactory)
class TestBottomUpEarlyTermScaleNonincremental extends TestScaleNonincremental("BottomUpEarlyTerm", BottomUpEarlyTermCheckerFactory)