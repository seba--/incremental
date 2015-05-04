package incremental.systemfomega

import constraints.normequality.impl.{SolveContinuousSubst, SolveContinuousSubstThreshold, SolveContinuously, SolveEnd}
import constraints.normequality.{ConstraintSystem, Type, UVar}
import incremental.Node._
import incremental.Util
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
 */
class TestCorrectness[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = {
    Util.log(f"Preparation time\t${checker.preparationTime}%.3fms")
    Util.log(f"Type-check time\t\t${checker.typecheckTime}%.3fms")
    Util.log(f"Constraint count\t${checker.constraintCount}")
    Util.log(f"Cons. solve time\t${checker.constraintSolveTime}%.3fms")
    Util.log(f"Merge reqs time\t\t${checker.mergeReqsTime}%.3fms")
    Util.log(f"Finalize time\t\t${checker.finalizeTime}%.3fms")
  }

  def typecheckTest(desc: String, e: =>Node)(expected: Type): Unit =
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

  typecheckTest("17", Num(17))(TNum)
  typecheckTest("17+(10+2)", Add(Num(17), Add(Num(10), Num(2))))(TNum)
  typecheckTest("17+(10+5)", Add(Num(17), Add(Num(10), Num(5))))(TNum)
  typecheckTest("\\x. 10+5", Abs('x, Add(Num(10), Num(5))))(TFun(UVar('x$0), TNum))
  typecheckTest("\\x. x+x", Abs('x, Add(Var('x), Var('x))))(TFun(TNum, TNum))
  typecheckTestError("\\x. err+x", Abs('x, Add(Var('err), Var('x))))
  typecheckTest("\\x. \\y. x y", Abs('x, Abs('y , App(Var('x), Var('y)))))(TFun(TFun(UVar('x$1), UVar('x$2)), TFun(UVar('x$1), UVar('x$2))))
  typecheckTest("\\x. \\y. x + y", Abs('x, Abs('y, Add(Var('x), Var('y)))))(TFun(TNum, TFun(TNum, TNum)))
  typecheckTest("if0(17, 0, 1)", If0(Num(17), Num(0), Num(1)))(TNum)

  // test polymorphism

  typecheckTestError("\\x. y", Abs('x, Var('y)))
  typecheckTestError("\\x: a . x", Abs('x, TVar.Kind('a), Var('x)))
  typecheckTest("\\a. \\x : a. x", TAbs('a, Abs('x, TVar.Kind('a), Var('x))))(TUniv('a, TFun(TVar('a), TVar('a))))
  typecheckTestError("\\a. \\x : a. x + x", TAbs('a, Abs('x, TVar.Kind('a), Add(Var('x), Var('x)))))
  typecheckTest("\\a. \\f : a -> a. \\x:a. f x", TAbs('a, Abs('f,TFun.Kind(TVar.Kind('a),TVar.Kind('a)),Abs('x, TVar.Kind('a), App(Var('f),Var('x))))))(TUniv('a,TFun(TFun(TVar('a), TVar('a)),TFun(TVar('a), TVar('a)))))
  typecheckTestError("\\a. \\b. \\f:a->a . \\x:b. f x", TAbs('a,TAbs('b, Abs('f, TFun.Kind(TVar.Kind('a), TVar.Kind('a)),Abs('x, TVar.Kind('b), App(Var('f),Var('x)))))))
  typecheckTestError("\\a. \\b. \\f:a . \\x:b. f x", TAbs('a,TAbs('b, Abs('f, TVar.Kind('a), Abs('x, TVar.Kind('b), App(Var('f),Var('x)))))))
  typecheckTest("(\\a. \\x : a. x) [Num]", TApp(TAbs('A, Abs('x, TVar.Kind('A), Var('x))), TNum.Kind()))(TFun(TNum,TNum))
  typecheckTest("\\b. (\\a. \\x : a. x) [b]", TAbs('B, TApp(TAbs('A, Abs('x, TVar.Kind('A), Var('x))), TVar.Kind('B))))(TUniv('B, TFun(TVar('B),TVar('B))))
  typecheckTestError("\\x. x [Num]", Abs('x, TApp(Var('x), TNum.Kind())))
  typecheckTestError("\\x. (x [Num]) + 1", Abs('x, Add(TApp(Var('x), TNum.Kind()), Num(1))))
  typecheckTestError("\\x:X. x", Abs('x, TVar.Kind('X), Var('x)))
  typecheckTestError("(\\X.\\x:X. x)[Y]", TApp(TAbs('X, Abs('x, TVar.Kind('X), Var('x))), TVar.Kind('Y)))
}

class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))

class TestDUSolveContniuouslyCorrectness extends TestCorrectness("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))

class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))

class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))

class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermCorrectness extends TestCorrectness("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
