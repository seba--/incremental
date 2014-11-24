package systemf

import incremental.Exp._
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import incremental.systemf._
/**
 * Created by seba on 14/11/14.
 */
class TestCorrectness(classdesc: String, checkerFactory: TypeCheckerFactory) extends FunSuite with BeforeAndAfterEach {
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

  typecheckTest("17", Num(17))(TNum)
  typecheckTest("17+(10+2)", Add(Num(17), Add(Num(10), Num(2))))(TNum)
  typecheckTest("17+(10+5)", Add(Num(17), Add(Num(10), Num(5))))(TNum)
  typecheckTest("\\x. 10+5", Abs('x, Add(Num(10), Num(5))))(TFun(TVar('x$0), TNum))
  typecheckTest("\\x. x+x", Abs('x, Add(Var('x), Var('x))))(TFun(TNum, TNum))
  typecheckTestError("\\x. err+x", Abs('x, Add(Var('err), Var('x))))
  typecheckTest("\\x. \\y. x y", Abs('x, Abs('y , App(Var('x), Var('y)))))(TFun(TFun(TVar('x$1), TVar('x$2)), TFun(TVar('x$1), TVar('x$2))))
  typecheckTest("\\x. \\y. x + y", Abs('x, Abs('y, Add(Var('x), Var('y)))))(TFun(TNum, TFun(TNum, TNum)))


  //typecheckTest("\\x: TNum . x", Abs('x, Var('x)))(TFun(TVar('x$0), TVar('x$0)))
  typecheckTest("\\x: TNum . x", Abs(Seq('x, TVar('a)),Seq(Var('x))))(TFun(TVar('x$0), TVar('x$0)))


  typecheckTest("\\a. \\x : a. x", TAbs('a, Abs(Seq('x,TUsVar('a)),Seq((Var('x))))))(TUniv('a, TFun(TUsVar('a), TUsVar('a))))

  //typecheckTest("\\a. \\x : a. x", TAbs('a, Abs(Seq('x,'a$0),(Var('x)))))(TUniv('a, TFun(TVar('x$0),TFun(TVar('x$3), TVar('x$0)))))
 typecheckTest("\\a. \\x : a. x + x", TAbs('a, Abs(Seq('x, TUsVar('a)), Seq(Add(Var('x), Var('x))))))(TUniv('a, TFun(TNum, TNum)))
  //typecheckTest("\\a. \\b. \\y. x y", Abs('x, Abs('y, App(Var('x), Var('y)))))(TFun(TFun(TVar('x$1), TVar('x$2)), TFun(TVar('x$1), TVar('x$2))))
 // typecheckTest("\\a. \\f . \\x:a. f x", TAbs('a, Abs(Seq('f,TFun(TUsVar('a),TUsVar('a))),Abs('x, App(Var('f),Var('x))))))(TUniv('a,TFun(TFun(TVar('x$1), TVar('x$2)),TFun(TVar('x$1), TVar('x$2)))))

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
}

//class TestDownUpCorrectness extends TestCorrectness("DownUp", DownUpCheckerFactory)

class TestBottomUpCorrectness extends TestCorrectness("BottomUp", BottomUpCheckerFactory)

//class TestBottomUpEarlyTermCorrectness extends TestCorrectness("BottomUpEarlyTerm", BottomUpEarlyTermCheckerFactory)