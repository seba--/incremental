package incremental.pcf

import incremental.Exp._
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
 */
class TestCorrectness(checkerFactory: TypeCheckerFactory) extends FunSuite with BeforeAndAfterEach {
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

  def typecheckTest(e: =>Exp)(expected: Type): Unit = typecheckTest(e.toString, e)(expected)
  def typecheckTest(desc: String, e: =>Exp)(expected: Type): Unit =
    test (s"Type check $desc") {
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
    }

  def typecheckTestError(e: =>Exp) =
    test (s"Type check $e") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest(Num(17))(TNum)
  typecheckTest(Add(Num(17), Add(Num(10), Num(2))))(TNum)
  typecheckTest(Add(Num(17), Add(Num(10), Num(5))))(TNum)
  typecheckTest(Abs('x, Add(Num(10), Num(5))))(TFun(TVar('x$0), TNum))
  typecheckTest(Abs('x, Add(Var('x), Var('x))))(TFun(TNum, TNum))
  typecheckTestError(Abs('x, Add(Var('err), Var('x))))
  typecheckTest(Abs('x, Abs('y, App(Var('x), Var('y)))))(TFun(TFun(TVar('x$1), TVar('x$2)), TFun(TVar('x$1), TVar('x$2))))
  typecheckTest(Abs('x, Abs('y, Add(Var('x), Var('y)))))(TFun(TNum, TFun(TNum, TNum)))
  typecheckTest(If0(Num(17), Num(0), Num(1)))(TNum)

  val fac = Fix(Abs('f, Abs('n, If0(Var('n), Num(1), Mul(Var('n), App(Var('f), Add(Var('n), Num(-1))))))))
  typecheckTest("factorial", fac)(TFun(TNum, TNum))
  typecheckTest("eta-expanded factorial", Abs('x, App(fac, Var('x))))((TFun(TNum, TNum)))

  val fib = Fix(Abs('f, Abs('n,
    If0(Var('n), Num(1),
      If0(Add(Var('n), Num(-1)), Num(1),
        Add(App(Var('f), Add(Var('n), Num(-1))),
          App(Var('f), Add(Var('n), Num(-2)))))))))
  typecheckTest("fibonacci", fib)(TFun(TNum, TNum))
  typecheckTest("factorial + fibonacci", Abs('x, Add(App(fac, Var('x)), App(fib, Var('x)))))(TFun(TNum, TNum))
  typecheckTest(Abs('y, Var('y)))(TFun(TVar('x$0), TVar('x$0)))
}

class TestBottomUpCorrectness extends TestCorrectness(BottomUpCheckerFactory)