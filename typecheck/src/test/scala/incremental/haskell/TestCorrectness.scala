package incremental.haskell

import constraints.CVar
import constraints.equality.impl.{SolveContinuously, SolveEnd}
import constraints.equality.{ConstraintSystem, Type}
import incremental.haskell.Node._
import org.scalatest.{BeforeAndAfterEach, FunSuite}


/**
 * Created by lira on 29/01/18.
 */

class TestCorrectness[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: BUCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: BUChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def typecheckTest(desc: String, e: =>Node)(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected $expected but got $actual")

      val sol = SolveContinuously.state.withValue(checker.csFactory.state.value) {
        expected.unify(actual.left.get, SolveContinuously.freshConstraintSystem).tryFinalize
        }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestFJ(desc: String, e: =>Node)(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val ev = e
      val actual = checker.typecheck(ev)

      val req = ev.withType[checker.Result].typ._2
      val creq = ev.withType[checker.Result].typ._3
      val sig = ev.withType[checker.Result].typ._4
      assert(actual.isLeft, s"Reqs = $req, CReqs = $creq, Signature = $sig ")

    }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

//  typecheckTest("17", Num(17))(TNum)
//  typecheckTest("17+(10+2)", Add(Num(17), Add(Num(10), Num(2))))(TNum)
//  typecheckTest("17+(10+5)", Add(Num(17), Add(Num(10), Num(5))))(TNum)
//  typecheckTest("\\x. 10+5", Abs('x, Add(Num(10), Num(5))))(TFun(UVar(CVar('x$0)), TNum))
//  typecheckTest("\\x. x+x", Abs('x, Add(Var('x), Var('x))))(TFun(TNum, TNum))
//  typecheckTestError("\\x. err+x", Abs('x, Add(Var('err), Var('x))))
//  typecheckTest("\\x. \\y. x y", Abs('x, Abs('y , FApp(Var('x), Var('y)))))(TFun(TFun(UVar(CVar('x$1)), UVar(CVar('x$2))), TFun(UVar(CVar('x$1)), UVar(CVar('x$2)))))
//  typecheckTest("\\x. \\y. x + y", Abs('x, Abs('y, Add(Var('x), Var('y)))))(TFun(TNum, TFun(TNum, TNum)))
//  typecheckTest("if0(17, 0, 1)", If(Num(17), Num(0), Num(1)))(TNum)
//
//
//  typecheckTest("let x = 1 in let y = 3 in x + y ", Let('x, CInt(3), Let('y, CInt(7), TAdd(Var('x), Var('y)))))(TInt)
//  typecheckTestError("let x = 1 in let y = 'a in x + y ", Let('x, CInt(3), Let('y, CChar('a), TAdd(Var('x), Var('y)))))
//  typecheckTestError("1 + a", TAdd(CInt(1), CChar('a)))

//  typecheckTest("inst zero = Int in  mul zero Int ", Inst('zero, TInt, CInt(0), TMul(Var('zero), CInt(2))))(TInt)
//  typecheckTestError("inst zero = Int in  mul zero Num ", Inst('zero, TFloat, CFloat(0), TMul(Var('zero), CChar('x))))
//  typecheckTestError(" Mul 1 (inst zero = Int 1 ", TMul( Var('zero), Inst('zero, TInt, CInt(0), CChar('x))))
//
//  typecheckTest("innst zero = int in inst zero = double in Abs('zero, Mul (zero Int) ) ",
//    Inst('zero, TInt, CInt(0), Inst('zero, TFloat, CFloat(0), TMul(Var('zero), CInt(2)))))(TInt)
//
//  typecheckTest("innst zero = int in inst zero = double in inst Zero = float in Abs('zero, Mul (zero Int) ) ",
//    Inst('zero, TInt, CInt(0), Inst('zero, TDouble, CDouble(0), Inst('zero, TFloat, CFloat(0), TMul(Var('zero), CInt(2))))))( TInt)
//
//  typecheckTestError("innst zero = int in inst zero = double in Abs('zero, Mul (zero Float) ) ",
//    Inst('zero, TInt, CInt(0), Inst('zero, TDouble, CDouble(0), TMul(Var('zero), CFloat(2)))))

// typecheckTest("[CFloat(0.0)] ",  ASeqExp(Lit(CChar('x)), NNone, NNone))(TFloat)
  typecheckTestError("[x, .. ] ",ASeqExp(Var('x), NNone, NNone))
  typecheckTestError("[x, x, .. ] ",ASeqExp(Var('x), NSome(Var('x)), NNone))


  typecheckTest("Lit(1)", Lit(Num(1)))(TNum)
}

//class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))
//class TestDUSolveContniuouslyCorrectness extends TestCorrectness("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermCorrectness extends TestCorrectness("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
