package incremental.pcf

import incremental.Exp._
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
 */
class TestCorrectness(classdesc: String, checkerFactory: TypeCheckerFactory) extends FunSuite with BeforeAndAfterEach {
  var checker: TypeChecker = checkerFactory.makeChecker

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
      assert(actual.isLeft, s"Expected $expected but got $actual")
      val sol = expected.unify(actual.left.get)
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: =>Exp) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest("17", Num(17))(TNum)
  typecheckTest("17+(10+2)", Add(Num(17), Add(Num(10), Num(2))))(TNum)
  typecheckTest("17+(10+5)", Add(Num(17), Add(Num(10), Num(5))))(TNum)
  typecheckTest("\\x. 10+5", Abs('x, Add(Num(10), Num(5))))(TFun(TVar('x$0), TNum))
  typecheckTest("\\x. x+x", Abs('x, Add(Var('x), Var('x))))(TFun(TNum, TNum))
  typecheckTestError("\\x. err+x", Abs('x, Add(Var('err), Var('x))))
  typecheckTest("\\x. \\y. x y", Abs('x, Abs('y, App(Var('x), Var('y)))))(TFun(TFun(TVar('x$1), TVar('x$2)), TFun(TVar('x$1), TVar('x$2))))
  typecheckTest("\\x. \\y. x + y", Abs('x, Abs('y, Add(Var('x), Var('y)))))(TFun(TNum, TFun(TNum, TNum)))
  typecheckTest("if0(17, 0, 1)", If0(Num(17), Num(0), Num(1)))(TNum)

  lazy val fac = Fix(Abs('f, Abs('n, If0(Var('n), Num(1), Mul(Var('n), App(Var('f), Add(Var('n), Num(-1))))))))
  typecheckTest("factorial", fac)(TFun(TNum, TNum))
  typecheckTest("eta-expanded factorial", Abs('x, App(fac, Var('x))))((TFun(TNum, TNum)))

  lazy val fib = Fix(Abs('f, Abs('n,
    If0(Var('n), Num(1),
      If0(Add(Var('n), Num(-1)), Num(1),
        Add(App(Var('f), Add(Var('n), Num(-1))),
          App(Var('f), Add(Var('n), Num(-2)))))))))
  typecheckTest("fibonacci", fib)(TFun(TNum, TNum))
  typecheckTest("factorial + fibonacci", Abs('x, Add(App(fac, Var('x)), App(fib, Var('x)))))(TFun(TNum, TNum))
  typecheckTest("\\y. y", Abs('y, Var('y)))(TFun(TVar('x$0), TVar('x$0)))
  typecheckTestError("\\x. x x",
    Abs(Seq('x, TVar('T)), Seq(App(Var('x), Var('x))))
  )
}

class TestDownUpCorrectness extends TestCorrectness("DownUp", DownUpCheckerFactory)
class TestDownUpSolveEndCorrectness extends TestCorrectness("DownUpSolveEnd", DownUpSolveEndCheckerFactory)
class TestBottomUpCorrectness extends TestCorrectness("BottomUp", BottomUpCheckerFactory)
class TestBottomUpSolveEndCorrectness extends TestCorrectness("BottomUpSolveEnd", BottomUpSolveEndCheckerFactory)
class TestBottomUpEarlyTermCorrectness extends TestCorrectness("BottomUpEarlyTerm", BottomUpEarlyTermCheckerFactory)
class TestBottomKeepSubstUpCorrectness extends TestCorrectness("BottomUpKeepSubst", BottomUpKeepSubstCheckerFactory)
