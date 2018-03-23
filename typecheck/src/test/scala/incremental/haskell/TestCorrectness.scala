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

  typecheckTest("CPoint(1, 2)", GConB(TCon('CPoint), Lit(CInt(1)), Lit(CInt(2))))(TCon('CPoint))
  typecheckTest("CPoint(1.0 , 2.0 )", GConB(TCon('CPoint), Lit(CFloat("1.0")), Lit(CFloat("2.0"))))(TCon('CPoint))

  typecheckTest("CPoint[1, 2]", GConS(TCon('CPoint), Lit(CInt(1)), Lit(CInt(2))))(TCon('CPoint))
  typecheckTest("CPoint{1, 2}", GConC(TCon('CPoint), Lit(CInt(1)), Lit(CInt(2))))(TCon('CPoint))

  typecheckTest("Lit(1)", Lit(CInt(1)))(TVar('a))
  typecheckTest("Lit(1.0)", Lit(CFloat("1.0")))(TVar('x$4))
  typecheckTest("Lit(1.00)", Lit(CRational("1.00")))(TVar('x$5))
  typecheckTest("Lit('a)", Lit(CChar('a)))(TChar)
  typecheckTest("Lit('Hello)", Lit(CString("Hello")))(ListH(TChar))


  typecheckTestError("( Var('x) )", PExp(Var('x)))
  typecheckTest("( Lit(1) )", PExp(Lit(CInt(1))))(TVar('a))

  typecheckTestError("( Var('x), Var('y) )", TupleExp(Var('x), Var('y)))
  typecheckTestError("( Var('x), Var('y), Lit(1), Lit(2) )", TupleExp(Var('x), Var('y), Lit(CInt(1)), Lit(CInt(2))))
  typecheckTest("(  Lit('a), Lit('b) )", TupleExp(Lit(CChar('a)), Lit(CChar('b))))(TupleH(List(TChar,ClassH('Char))))
  typecheckTest("(  Lit('a), Lit('b), Lit(1) )", TupleExp(Lit(CChar('a)), Lit(CChar('b)), Lit(CInt(1))))(TupleH(List(TChar,TChar, TVar('a))))
  typecheckTestError("[ Var('x), Var('y) ]", LExp(Var('x), Var('y)))
  typecheckTest("[ Lit('a), Lit('b), Lit('c)]", LExp(Lit(CChar('a)), Lit(CChar('b)), Lit(CChar('c))))(ListH(ClassH('Char)))


  typecheckTest("[a, .. ] ",ASeqExp(CChar('a), NNone, NNone))(SeqH(ClassH('Char)))
  typecheckTest("[a, c, .. ] ",ASeqExp(CChar('a), NSome(CChar('c)), NNone))(SeqH(ClassH('Char)))
  typecheckTest("[a .. f ] ",ASeqExp(CChar('a), NNone, NSome(CChar('f))))(SeqH(ClassH('Char)))
  typecheckTest("[a, c, .. z] ",ASeqExp(CChar('a), NSome(CChar('c)), NSome(CChar('z))))(SeqH(ClassH('Char)))
  typecheckTestError("[a, c, .. 10] ",ASeqExp(CChar('a), NSome(CChar('c)), NSome(CInt(10))))

  typecheckTest("[5, .. ] ",ASeqExp(Lit(CInt(5)), NNone, NNone))(SeqH(TVar('a)))
  typecheckTest("[5, 7, .. ] ",ASeqExp(Lit(CInt(5)), NSome(Lit(CInt(7))), NNone))(SeqH(TVar('a)))
  typecheckTest("[5 .. 15 ] ",ASeqExp(Lit(CInt(5)), NNone, NSome(Lit(CInt(15)))))(SeqH(TVar('a)))
  typecheckTest("[5, 7, .. 25] ",ASeqExp(Lit(CInt(5)), NSome(Lit(CInt(5))), NSome(Lit(CInt(5)))))(SeqH(TVar('a)))
  typecheckTestError("[5, 'd, .. 25] ",ASeqExp(Lit(CInt(5)), NSome(Lit(CChar('d))), NSome(Lit(CInt(5)))))

  typecheckTest("1 + 2", TAdd(Lit(CInt(1)), Lit(CInt(2))))(TVar('a))
  typecheckTest("let {x = 2; y = 3} in x + y ", Let(VarDecl('x, Lit(CInt(2))), VarDecl('y, Lit(CInt(3))), TAdd(Var('x), Var('y))))(TVar('a))
  typecheckTestError("let {x = 1;  y = 'a} in x + y ", Let(VarDecl('x, Lit(CInt(2))), VarDecl('x, Lit(CChar('a))), TAdd(Var('x), Var('y))))

  typecheckTestError("x [@None]", VarP('x, NNone))
  typecheckTest("Literal Pat => Char('a)", LitP(CChar('a)))(TChar)

  typecheckTest("case 2 of {(1)->A;(2)->B;(3)->C}", Case(Lit(CInt(2)), Alt(LitP(CInt(1)), Lit(CChar('A))),
    Alt(LitP(CInt(2)), Lit(CChar('B))), Alt(LitP(CInt(3)), Lit(CChar('C)))))(TChar)
  typecheckTestError("case 2 of {(1)->A;(2)->0;(3)->C}", Case(Lit(CInt(2)), Alt(LitP(CInt(1)), Lit(CChar('A))),
    Alt(LitP(CInt(2)), Lit(CInt(0))), Alt(LitP(CInt(3)), Lit(CChar('C)))))
  typecheckTest("case 'a of {('c)->3;('b)->2;('a)->1}", Case(Lit(CChar('a)), Alt(LitP(CChar('c)), Lit(CInt(3))),
    Alt(LitP(CChar('b)), Lit(CInt(2))), Alt(LitP(CChar('a)), Lit(CInt(1)))))(TVar('a))
  typecheckTestError("case 'a of {('c)-> 'a;('b)->2;('a)->1}", Case(Lit(CChar('a)), Alt(LitP(CChar('c)), Lit(CChar('a))),
    Alt(LitP(CChar('b)), Lit(CInt(2))), Alt(LitP(CChar('a)), Lit(CInt(1)))))

  typecheckTest(" let x = 2 in case x of {(1)->A;(2)->B;(3)->C}", Let(VarDecl('x, Lit(CInt(2))), Case(Var('x), Alt(LitP(CInt(1)), Lit(CChar('A))),
    Alt(LitP(CInt(2)), Lit(CChar('B))), Alt(LitP(CInt(3)), Lit(CChar('C))))))(TChar)
  typecheckTestError(" let x = 'a in case x of {(1)->A;(2)->B;(3)->C}", Let(VarDecl('x, Lit(CChar('a))), Case(Var('x), Alt(LitP(CInt(1)), Lit(CChar('A))),
    Alt(LitP(CInt(2)), Lit(CChar('B))), Alt(LitP(CInt(3)), Lit(CChar('C))))))

  typecheckTest("if 5 < = 3 then 5 + 3 else 5 x 3", If(Leq(Lit(CInt(5)),Lit(CInt(3))),
    TAdd(Lit(CInt(5)),Lit(CInt(3))),
    TMul(Lit(CInt(5)),Lit(CInt(3)))))(TVar('a))

  typecheckTestError("if 5 < = 3 then 5 + 3 else Char('a)", If(Leq(Lit(CInt(5)),Lit(CInt(3))),
    TAdd(Lit(CInt(5)),Lit(CInt(3))),
    Lit(CChar('a))))

  typecheckTest(" let x = 1; y = 3 in if x < = y then x + y else y x x", Let(VarDecl('x, Lit(CInt(1))), VarDecl('y, Lit(CInt(3))),
    If(Leq(Var('x), Var('y)),
    TAdd(Var('x), Var('y)),
    TMul(Var('x), Var('y)))))(TVar('a))

  typecheckTestError(" let x = 1; y = 'a in if x < = y then x + y else y x x", Let(VarDecl('x, Lit(CInt(1))), VarDecl('y, Lit(CChar('a))),
    If(Leq(Var('x), Var('y)),
      TAdd(Var('x), Var('y)),
      TMul(Var('x), Var('y)))))

  typecheckTest("ExpStmt Lit(Char('a))", ExpStmt(Lit(CChar('a))))(TChar)
  typecheckTestError("ExpStmt 'x", ExpStmt(Var('x)))

  typecheckTestError("patstmt x <- Char(a)", PatStmt(VarP('x), Lit(CChar('a))))
  typecheckTest("patstmt CInt(1) <- Char(a)", PatStmt(LitP(CInt(1)), Lit(CChar('a))))(TChar)

  typecheckTest("let x = 1, y = 2", LetStmt(VarDecl('x, Lit(CInt(1))), VarDecl('x, Lit(CInt(1)))))(TupleH(List(TVar('a), TVar('a))))
  typecheckTest("let x = 1, y = 'a", LetStmt(VarDecl('x, Lit(CInt(1))), VarDecl('x, Lit(CChar('b)))))(TupleH(List(TVar('a), ClassH('Char))))
  typecheckTest("Do Lit('a); Lit(1) Lit('c)", Do(ExpStmt(CChar('a)), ExpStmt(CInt(1)), CChar('c)))(TChar)
  typecheckTest("Do let x = 1; y = 2 Add(x, y)", Do(LetStmt(VarDecl('x, Lit(CInt(1))), VarDecl('y, Lit(CInt(1)))), TAdd(Var('x), Var('y))))(TVar('a))

}


//class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))
//class TestDUSolveContniuouslyCorrectness extends TestCorrectness("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermCorrectness extends TestCorrectness("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
