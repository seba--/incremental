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

  typecheckTestError("x", Var('x))

  typecheckTest("CPoint(1, 2)", GConB(TCon('CPoint), Lit(Num(1)), Lit(Num(2))))(TCon('CPoint))
  typecheckTest("CPoint(1.0 , 2.0 )", GConB(TCon('CPoint), Lit(CFloat("1.0")), Lit(CFloat("2.0"))))(TCon('CPoint))

  typecheckTest("CPoint[1, 2]", GConS(TCon('CPoint), Lit(Num(1)), Lit(Num(2))))(TCon('CPoint))
  typecheckTest("CPoint{1, 2}", GConC(TCon('CPoint), Lit(Num(1)), Lit(Num(2))))(TCon('CPoint))

  typecheckTest("Lit(1)", Lit(Num(1)))(TVar('a))
  typecheckTest("Lit(1.0)", Lit(CFloat("1.0")))(TVar('x$4))
  typecheckTest("Lit(1.00)", Lit(CReal("1.00")))(TVar('x$5))
  typecheckTest("Lit(2)", Lit(CInt(2)))(TVar('x$6))
  typecheckTest("Lit('a)", Lit(CChar('a)))(ClassH('Char))

  typecheckTestError("( Var('x) )", PExp(Var('x)))
  typecheckTest("( Lit(1) )", PExp(Lit(Num(1))))(TVar('a))

  typecheckTestError("( Var('x), Var('y) )", TupleExp(Var('x), Var('y)))
  typecheckTestError("( Var('x), Var('y), Lit(1), Lit(2) )", TupleExp(Var('x), Var('y), Lit(Num(1)), Lit(Num(2))))
  typecheckTest("(  Lit('a), Lit('b) )", TupleExp(Lit(CChar('a)), Lit(CChar('b))))(TupleH(List(ClassH('Char),ClassH('Char))))
  typecheckTest("(  Lit('a), Lit('b), Lit(1) )", TupleExp(Lit(CChar('a)), Lit(CChar('b)), Lit(Num(1))))(TupleH(List(ClassH('Char),ClassH('Char), TVar('a))))
  typecheckTestError("[ Var('x), Var('y) ]", LExp(Var('x), Var('y)))
  typecheckTest("[ Lit('a), Lit('b), Lit('c)]", LExp(Lit(CChar('a)), Lit(CChar('b)), Lit(CChar('c))))(ListH(ClassH('Char)))


  typecheckTest("[a, .. ] ",ASeqExp(CChar('a), NNone, NNone))(SeqH(ClassH('Char)))
  typecheckTest("[a, c, .. ] ",ASeqExp(CChar('a), NSome(CChar('c)), NNone))(SeqH(ClassH('Char)))
  typecheckTest("[a .. f ] ",ASeqExp(CChar('a), NNone, NSome(CChar('f))))(SeqH(ClassH('Char)))
  typecheckTest("[a, c, .. z] ",ASeqExp(CChar('a), NSome(CChar('c)), NSome(CChar('z))))(SeqH(ClassH('Char)))
  typecheckTestError("[a, c, .. 10] ",ASeqExp(CChar('a), NSome(CChar('c)), NSome(Num(10))))

  typecheckTest("[5, .. ] ",ASeqExp(Lit(Num(5)), NNone, NNone))(SeqH(TVar('a)))
  typecheckTest("[5, 7, .. ] ",ASeqExp(Lit(Num(5)), NSome(Lit(Num(7))), NNone))(SeqH(TVar('a)))
  typecheckTest("[5 .. 15 ] ",ASeqExp(Lit(Num(5)), NNone, NSome(Lit(Num(15)))))(SeqH(TVar('a)))
  typecheckTest("[5, 7, .. 25] ",ASeqExp(Lit(Num(5)), NSome(Lit(Num(5))), NSome(Lit(Num(5)))))(SeqH(TVar('a)))
  typecheckTestError("[5, 'd, .. 25] ",ASeqExp(Lit(Num(5)), NSome(Lit(CChar('d))), NSome(Lit(Num(5)))))

  typecheckTest("1 + 2", TAdd(Lit(Num(1)), Lit(Num(2))))(TVar('a))
  typecheckTest("let {x = 2; y = 3} in x + y ", Let(VarDecl('x, Lit(Num(2))), VarDecl('y, Lit(Num(3))), TAdd(Var('x), Var('y))))(TVar('a))
  typecheckTestError("let {x = 1;  y = 'a} in x + y ", Let(VarDecl('x, Lit(Num(2))), VarDecl('x, Lit(CChar('a))), TAdd(Var('x), Var('y))))


}

//class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))
//class TestDUSolveContniuouslyCorrectness extends TestCorrectness("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermCorrectness extends TestCorrectness("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
