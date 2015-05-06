package incremental.pcf.with_records

import constraints.equality.impl.{SolveContinuousSubstThreshold, SolveContinuousSubst, SolveEnd, SolveContinuously}
import constraints.equality.{Type, ConstraintSystem}
import incremental.Node._
import incremental.Util
import incremental.pcf._
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
 */
class TestCorrectness[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def typecheckTest(desc: String, e: =>Node)(expected: Type) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected $expected but got $actual")

      val sol = SolveContinuously.state.withValue(checker.csFactory.state.value) {
        expected.unify(actual.left.get, SolveContinuously.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest("Record(l:17)", Record('l, Num(17)))(TRecord(Map('l -> TNum)))
  typecheckTest("Record(l:17, k:\\x. x+x)", Record(Seq('l, 'k), Seq(Num(17), Abs('x, Add(Var('x), Var('x))))))(TRecord(Map('l -> TNum, 'k -> TFun(TNum, TNum))))
  typecheckTest("Record(l:17, k:\\x. x+x).l", Project('l, Record(Seq('l, 'k), Seq(Num(17), Abs('x, Add(Var('x), Var('x)))))))(TNum)
  typecheckTest("Record(l:17, k:\\x. x+x).k", Project('k, Record(Seq('l, 'k), Seq(Num(17), Abs('x, Add(Var('x), Var('x)))))))(TFun(TNum, TNum))
  typecheckTest("\\x. x.m + x.m", Abs('x, Add(Project('m, Var('x)), Project('m, Var('x)))))(TFun(TRecord(Map('m -> TNum)), TNum))
  typecheckTest("\\x. x.m + x.n", Abs('x, Add(Project('m, Var('x)), Project('n, Var('x)))))(TFun(TRecord(Map('m -> TNum, 'n -> TNum)), TNum))
  typecheckTestError("\\x. x.m + (x.m) 0", Abs('x, Add(Project('m, Var('x)), App(Project('m, Var('x)), Num(0)))))
}

class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))
class TestDUSolveContniuouslyCorrectness extends TestCorrectness("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermCorrectness extends TestCorrectness("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
