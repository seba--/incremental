package incremental.pcf.with_references

import constraints.equality.impl.{SolveContinuousSubstThreshold, SolveContinuousSubst, SolveEnd, SolveContinuously}
import constraints.equality.{UVar, Type, ConstraintSystem}
import incremental.Node._
import incremental.pcf._
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


  typecheckTest("Ref(17)", Ref(Num(17)))(TRef(TNum))
  typecheckTest("17+Deref(Ref(10+2))", Add(Num(17), Deref(Ref(Add(Num(10), Num(2))))))(TNum)
  typecheckTest("\\x. Deref(x)+Deref(x)", Abs('x, Add(Deref(Var('x)), Deref(Var('x)))))(TFun(TRef(TNum), TNum))
  typecheckTestError("\\x. x+Deref(x)", Abs('x, Add(Var('x), Deref(Var('x)))))
  val V = UVar('VAR_V)
  typecheckTest("\\x. \\y. x=y", Abs('x, Abs('y, Assign(Var('x), Var('y)))))(TFun(TRef(V), TFun(V, TUnit)))
  val W = UVar('VAR_W)
  typecheckTest("\\x. \\y. x=y; y", Abs('x, Abs('y, Seq(Assign(Var('x), Var('y)), Var('y)))))(TFun(TRef(W), TFun(W, W)))
}

class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))
class TestDUSolveContniuouslyCorrectness extends TestCorrectness("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermCorrectness extends TestCorrectness("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
