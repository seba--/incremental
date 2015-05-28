package incremental.FJava

import constraints.CVar
import constraints.equality.impl._
import constraints.equality._
import incremental.Node._
import incremental.{ Node_, Util}
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

  def typecheckTestFJ(desc: String, e: =>Node)(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val ev = e
      val actual = checker.typecheck(ev)

      val req = ev.withType[checker.Result].typ._2
      val creq = ev.withType[checker.Result].typ._3
      assert(actual.isLeft, s"Reqs = $req and CReqs = $creq ")

    }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }


  typecheckTestFJ("x", Var('x))(UCName(CVar('x)))
  typecheckTestFJ("e0.f : C ", Fields('f,Var('e0)))(UCName(CVar('C)))
  typecheckTestFJ("new C(x):C", New(Seq(CName('c)),Seq(Var('x))))(CName('c))
  typecheckTestFJ("e0.m() : void", Invk(Seq('m), Seq(Var('e0))))(UCName(CVar('void)))
  typecheckTestFJ("e0.m(e) : Double", Invk(Seq('m), Seq(Var('e0), Var('e))))(UCName(CVar('Double)))
  typecheckTestFJ("Point.add(p1, p2) : Point", Invk(Seq('add), Seq(Var('Point),Var('p1), Var('p2))))(UCName(CVar('Point)))
  typecheckTestError("(C) e0 :C", UCast(CName('c),Var('e)))
  typecheckTestError("y", Var('x))
  typecheckTestError("new Pair(first) : Pair", New(CName('Pair),Var('first)))
  typecheckTestError("new Pair(snd): Pair", New(CName('Pair),Var('object)))
  typecheckTestFJ("Pair.setfst(first) : U ", Invk(Seq('setfst),Seq(New(CName('Pair),Var('first)), Var('first))))(CName('int))
  typecheckTestError("(Object)first : Object", UCast(CName('Object),Var('first)))
  typecheckTestError("(Pair) first : Pair", New(CName('Pair),Var('first)))
  typecheckTestError("(Pair) first : Pair, second : Pair", New(CName('Pair),Var('first), Var('second)))
  typecheckTestFJ("new Object()", New(CName('Object)))(CName('Object))
  typecheckTestFJ("new Pair(fst : First, snd : Second)", New(Seq(CName('Pair)), Seq(Var('First), Var('Second))))(CName('Pair))

  typecheckTestFJ(" (new Pair(first)).first : Int", Fields('f,New(CName('Pair),Var('f))))(UCName(CVar('Int)))


  typecheckTestFJ("'void m() in C", MethodDec(Seq(CName('C),CName('void),'m,Seq(('x,UCName(CVar('x))),('y,UCName(CVar('y))))),Seq(New(CName('Object)))))(CName('C))
  typecheckTestFJ("'Double m( e0.m(x) ) in C", MethodDec(Seq(CName('C),CName('void),'m,Seq(('C$0,UCName(CVar('C$0))))),Seq(Invk(Seq('m), Seq(Var('e0), Var('C$0))))))(CName('C))
  typecheckTestFJ("'Double ) ) in Pair", MethodDec(Seq(CName('Pair),CName('void),'m,Seq(('f,UCName(CVar('f))))),Seq(Invk(Seq('setfst),Seq(New(CName('Pair),Var('f)), Var('f))))))(CName('Pair))
  typecheckTestFJ("' in Pair", MethodDec(Seq(CName('Point),CName('Double),'getSum,Seq(('fst,UCName(CVar('fst))))),Seq(Invk(Seq('add),Seq(Var('Point),Var('fst), Var('snd))))))(CName('Double))
  typecheckTestFJ("Adding two points", MethodDec(Seq(CName('Point),CName('Double),'getSum,Seq(('fst,UCName(CVar('fst))), ('snd, UCName(CVar('snd))))),Seq(Invk(Seq('add),Seq(Var('Point),Var('fst), Var('snd))))))(CName('Point))


  //typecheckTestError("(C) e0 : C", DCast(CName('c),Var('e)))
  //typecheckTestError("(C) e0 : C", SCast(CName('c),'e))
  typecheckTestFJ("Int getX(x: Int) {return Int} in Number", MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(New(CName('Int)))))(CName('Number))
  typecheckTestFJ("Int getX(x: Int) {Number.getX(x): Int} in Number", MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(Invk(Seq('getX),Seq(Var('Number),Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Nr).getX(x): Int} in Number", MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(Invk(Seq('getX),Seq(New(CName('Nr)),Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number).gX(x): Int} in Number", MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(Invk(Seq('gX),Seq(New(CName('Number)),Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number(x)).getX(x): Int} in Number", MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(Invk(Seq('getX),Seq(New(CName('Number), Var('x)),Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number).getX(x): Int} in Number", MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(Invk(Seq('getX),Seq(New(CName('Number)),Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number(x).x : Int in Number}", MethodDec(Seq(CName('Number), CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Fields('x, New(CName('Number),Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number(x,y).x : Int in Number}", MethodDec(Seq(CName('Number), CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Fields('x, New(CName('Number),Var('x),Var('y))))))(CName('Number))
  // /typecheckTestError("Int getXY(x,y) {return Int} in Number", Method(CName('Number), CName('Int), 'getXY, Seq('x, 'y),Var('e0)))


  typecheckTestFJ("e0.x + e0.x : TNum", Add(Fields('f,Num(1)),Fields('f,Num(1))))(TNum)
  typecheckTestFJ("e0.x + e0.x : C", Add(Fields('f,Num(1)),Fields('f,Num(1))))(CName('C))

}



class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
