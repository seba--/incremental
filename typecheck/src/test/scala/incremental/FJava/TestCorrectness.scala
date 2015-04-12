package incremental.FJava

import incremental.Exp._
import incremental.FJava._
import incremental.{Util, TypeChecker, Type, TypeCheckerFactory}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by lirakuci on 3/29/15.
 */
class TestCorrectness(classdesc: String, checkerFactory: TypeCheckerFactory[Type]) extends FunSuite with BeforeAndAfterEach {
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
  }

  def typecheckTest(desc: String, e: =>Exp)(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
    }

  def typecheckTestError(desc: String, e: =>Exp) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTestError("x", Var('x))
  typecheckTestError("e0.f : U ", Field(CName('e0),Var('f)))
  typecheckTestError("new C(x):C", New(CName('c),Var('x)))

}

class TestBottomUpCorrectness extends TestCorrectness("BottomUp FJava", BottomUpCheckerFactory)

