package incremental.FJava

import constraints.CVar
import constraints.equality.impl._
import constraints.equality._
import incremental.Node._
import incremental.systemf.UVar
import incremental.{Node_, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by lirakuci on 3/29/15.
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

  def typecheckTestFJ(desc: String, e: =>Node_[checker.Result])(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e.withType[Any])
      assert(actual.isLeft, s"Expected $expected but got $actual")
      e.typ._3
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


  typecheckTestFJ("x", Var('x))(UVar(CVar('x$1)))
  typecheckTestError("e0.f : U ", Fields('f,Var('e0)))
  typecheckTestError("new C(x):C", New(CName('c),Var('x)))
  typecheckTestError("e0.m()", Invk(Seq('m), Seq(Var('e0))))
  typecheckTestError("e0.m(e) : U", Invk(Seq('m), Seq(Var('e0), Var('e))))
  typecheckTestError("Pair.m(e1, e2)", Invk(Seq('m), Seq(Var('pair),Var('e1), Var('e2))))
  typecheckTestError("(C) e0 :C", UCast(CName('c),Var('e)))
  typecheckTestError("y", Var('x))
  typecheckTestError(" (new Pair(first)).first : U", Fields('first,New(CName('Pair),Var('first))))
  typecheckTestError("new Pair(first) : Pair", New(CName('Pair),Var('first)))
  typecheckTestError("new Pair(snd): Pair", New(CName('Pair),Var('object)))
  typecheckTestError("Pair.setfst(first) : U ", Invk(Seq('setfst),Seq(New(CName('Pair),Var('first)), Var('first))))
  typecheckTestError("(Object)first : Object", UCast(CName('Object),Var('first)))
  typecheckTestError("(Pair) first : Pair", New(CName('Pair),Var('first)))
  typecheckTestError("(Pair) first : Pair, second : Pair", New(CName('Pair),Var('first), Var('second)))
  typecheckTestError("new Object()", New(CName('Object)))
  typecheckTestError("new Pair(fst : First, snd : Second)", New(CName('Pair), Seq(Var('First), Var('Second))))
  //typecheckTestError("(C) e0 : C", DCast(CName('c),Var('e)))
  //typecheckTestError("(C) e0 : C", SCast(CName('c),'e))
  //typecheckTestError("Int getX(x: Int) {return Int} in Number", Method(Seq(CName('Number), CName('Int), 'getX, 'x, Var('int), 'y),Seq(Var('e0))))
//  //typecheckTestError("Int getXY(x,y) {return Int} in Number", Method(CName('Number), CName('Int), 'getXY, Seq('x, 'y),Var('e0)))
}


class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
