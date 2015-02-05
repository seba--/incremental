package incremental.systemf

import incremental.Exp._
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import incremental.systemf._
/**
 * Created by seba on 14/11/14.
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

  typecheckTest("17", Num(17))(TNum)
  typecheckTest("17+(10+2)", Add(Num(17), Add(Num(10), Num(2))))(TNum)
  typecheckTest("17+(10+5)", Add(Num(17), Add(Num(10), Num(5))))(TNum)
  typecheckTest("\\x. 10+5", Abs('x, Add(Num(10), Num(5))))(TFun(UVar('x$0), TNum))
  typecheckTest("\\x. x+x", Abs('x, Add(Var('x), Var('x))))(TFun(TNum, TNum))
  typecheckTestError("\\x. err+x", Abs('x, Add(Var('err), Var('x))))
  typecheckTest("\\x. \\y. x y", Abs('x, Abs('y , App(Var('x), Var('y)))))(TFun(TFun(UVar('x$1), UVar('x$2)), TFun(UVar('x$1), UVar('x$2))))
  typecheckTest("\\x. \\y. x + y", Abs('x, Abs('y, Add(Var('x), Var('y)))))(TFun(TNum, TFun(TNum, TNum)))
  //typecheckTest("if0(17, 0, 1)", If0(Num(17), Num(0), Num(1)))(TNum)

//  lazy val fac = Fix(Abs('f, Abs('n, If0(Var('n), Num(1), Mul(Var('n), App(Var('f), Add(Var('n), Num(-1))))))))
//  typecheckTest("factorial", fac)(TFun(TNum, TNum))
//  typecheckTest("eta-expanded factorial", Abs('x, App(fac, Var('x))))((TFun(TNum, TNum)))
//
//  lazy val fib = Fix(Abs('f, Abs('n,
//    If0(Var('n), Num(1),
//      If0(Add(Var('n), Num(-1)), Num(1),
//        Add(App(Var('f), Add(Var('n), Num(-1))),
//          App(Var('f), Add(Var('n), Num(-2)))))))))
//  typecheckTest("fibonacci", fib)(TFun(TNum, TNum))
//  typecheckTest("factorial + fibonacci", Abs('x, Add(App(fac, Var('x)), App(fib, Var('x)))))(TFun(TNum, TNum))
//  typecheckTest("\\y. y", Abs('y, Var('y)))(TFun(TVarInternal('x$0), TVarInternal('x$0)))

  // test polymorphism

  //typecheckTest("\\a. \\x: a. \\f:b. x" ,(Abs('x, TVar('a), App(Var('x) ,App(TVar('a), Var('x))))))(TFun(TFun(TVar('a), TVar('a)),TFun((TVar('a)), (TVar('a)))))
  //(TFun(TFun((TFun(TVar('a), TVar('a))),(TVar('a))), (TVar('a))))

  typecheckTestError("\\x.y", Abs('x, Var('y)))

  typecheckTestError("\\x: a . x", TAbs('x, TVar('a), Var('x)))

  typecheckTest("\\a. \\x : a. x", TAbs('a, Abs('x, TVar('a), Var('x))))(TUniv('a, TFun(TVar('a), TVar('a))))
  typecheckTestError("\\a. \\x : a. x + x", TAbs('a, Abs('x, TVar('a), Add(Var('x), Var('x)))))
  typecheckTest("\\a. \\f : a -> a. \\x:a. f x", TAbs('a, Abs('f,TFun(TVar('a),TVar('a)),Abs('x, TVar('a), App(Var('f),Var('x))))))(TUniv('a,TFun(TFun(TVar('a), TVar('a)),TFun(TVar('a), TVar('a)))))
  typecheckTestError("\\a. \\b. \\f:a->a . \\x:b. f x", TAbs('a,TAbs('b, Abs('f,TFun(TVar('a),TVar('a)),Abs('x, TVar('b), App(Var('f),Var('x)))))))
  typecheckTestError("\\a. \\b. \\f:a . \\x:b. f x", TAbs('a,TAbs('b, Abs('f,TVar('a),Abs('x, TVar('b), App(Var('f),Var('x)))))))
  //  typecheckTest("\\a. \\b. \\f:a->b .  \\c. \\g. : b->c \\x:a.g f x", TAbs('a,TAbs('b, Abs(Seq('f,TFun(TUsVar('a),TUsVar('a))),Seq(Abs(Seq('x, TUsVar('b)), Seq(App(Var('f),Var('x)))))))))(TUniv('a,TUniv('b,TFun(TFun(TUsVar('a), TUsVar('x$0)),TFun(TUsVar('b), TUsVar('x$0))))))
  //typecheckTest("\\a-> TNum. \\x : a. x", TAbs('a, TApp(TNum,Abs('x, TVar('a), Var('x)))))(TUniv('a, TFun(TFun(TVar('a), TNum),TFun(TNum,TNum))))
  typecheckTest("(\\a. \\x : a. x) [Num]", TApp(TNum, TAbs('A, Abs('x, TVar('A), Var('x)))))(TFun(TNum,TNum))
  typecheckTest("\\b. (\\a. \\x : a. x) [b]", TAbs('B, TApp(TVar('B), TAbs('A, Abs('x, TVar('A), Var('x))))))(TUniv('B, TFun(TVar('B),TVar('B))))
  typecheckTestError("\\x. x [Num]", Abs('x, TApp(TNum, Var('x))))
  typecheckTestError("\\x. (x [Num]) + 1", Abs('x, Add(TApp(TNum, Var('x)), Num(1))))

  //typecheckTest("\\x: \\a.a->a. x \\a.a->a x" ,(Abs('x, TAbs('a, TFun(TVar('a),TVar('a))), App(Var('x), App(TAbs('a, TFun(TVar('a),TVar('a))),Var('x))))))(TFun((TUniv('a, TFun(TVar('a), TVar('a)))),(TUniv('a, TFun(TVar('a), TVar('a))))))

  typecheckTestError("\\x:X. x", Abs('x, TVar('X), Var('x)))
  typecheckTestError("(\\X.\\x:X. x)[Y]", TApp(TVar('Y), TAbs('X, Abs('x, TVar('X), Var('x)))))
}

class TestDownUpCorrectness extends TestCorrectness("DownUp", DownUpCheckerFactory)
class TestBottomUpCorrectness extends TestCorrectness("BottomUp", BottomUpCheckerFactory)

//class TestBottomUpEarlyTermCorrectness extends TestCorrectness("BottomUpEarlyTerm", BottomUpEarlyTermCheckerFactory)