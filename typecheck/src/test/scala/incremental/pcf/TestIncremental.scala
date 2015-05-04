package incremental.pcf

import constraints.{CVar, equality}
import constraints.equality.impl.{SolveContinuousSubstThreshold, SolveContinuousSubst, SolveContinuously, SolveEnd}
import constraints.equality.ConstraintSystem
import incremental.Node._
import incremental.Util
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
 */
class TestIncremental[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  var checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = {
    Util.log(f"Preparation time\t${checker.preparationTime}%.3fms")
    Util.log(f"Type-check time\t\t${checker.typecheckTime}%.3fms")
    Util.log(f"Constraint count\t${checker.constraintCount}")
    Util.log(f"Cons. solve time\t${checker.constraintSolveTime}%.3fms")
    Util.log(f"Merge reqs time\t\t${checker.mergeReqsTime}%.3fms")
    Util.log(f"Finalize time\t\t${checker.finalizeTime}%.3fms")
  }

  def incTypecheckTest(desc: String, e: =>Node)(expected: equality.Type)(consCount: Int): Unit = incTypecheckTest(desc, e, Unit)(expected)(consCount)
  def incTypecheckTest(desc: String, e: =>Node, mod: =>Unit)(expected: equality.Type)(consCount: Int): Unit =
    test (s"$classdesc: Type check $desc") {
      val Unit = mod
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
      assertResult(consCount, "solved constraint(s)")(checker.constraintCount)
    }

  def typecheckTestError(e: =>Node) =
    test (s"$classdesc: Type check $e") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  lazy val e10 = Num(10)
  incTypecheckTest("10", e10)(TNum)(0)
  lazy val add1 = Add(e10, Num(2))
  incTypecheckTest("10+2", add1)(TNum)(2)
  lazy val add2 = Add(e10, add1)
  incTypecheckTest("10+(10+2)", add2)(TNum)(2)
  incTypecheckTest("10+(10+5)", add2, add1.kids(1) = Num(5))(TNum)(4)
  lazy val add3 = Add(Num(19), Num(20))
  incTypecheckTest("19+20", add3)(TNum)(2)
  incTypecheckTest("10+((19+20)+5)", add2, add1.kids(0) = add3)(TNum)(4)
  lazy val add4 = Add(Num(37), Num(49))
  incTypecheckTest("37+40", add4)(TNum)(2)
  incTypecheckTest("10+((19+20)+(37+40))", add2, add1.kids(1) = add4)(TNum)(4)
  incTypecheckTest("add4 again", add4)(TNum)(0)

  lazy val fun1 = Abs('x, add4)
  incTypecheckTest("\\x. add4", fun1)(TFun(UVar(CVar('x$0)), TNum))(0)
  incTypecheckTest("\\x. x + x", fun1, fun1.kids(0) = Add(Var('x), Var('x)))(TFun(TNum, TNum))(3)
  lazy val app1 = Add(App(fun1, Num(3)), Add(Num(1), Num(2)))
  incTypecheckTest("(\\x. x+x) 3 + (1 + 2)", app1)(TNum)(5)
  incTypecheckTest("(\\x. x+x+x) 3 + (1 + 2)", app1, fun1.kids(0) = Add(fun1.kids(0), Var('x)))(TNum)(6)

  val fac = Fix(Abs('f, Abs('n, If0(Var('n), Num(0), Mul(Var('n), App(Var('f), Add(Var('n), Num(-1))))))))
  incTypecheckTest("factorial base 0", fac)(TFun(TNum, TNum))(10)
  incTypecheckTest("factorial base 1", fac, fac match {case Fix(Abs(Abs(e@If0(_,_,_)))) => e.kids(1) = Num(1)})(TFun(TNum, TNum))(4)
}

class TestBUSolveEndIncremental extends TestIncremental("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuouslyIncremental extends TestIncremental("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
class TestBUSolveContinuousSubstIncremental extends TestIncremental("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuousSubstThresholdIncremental extends TestIncremental("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
