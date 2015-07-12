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

  def typecheckTest(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected $expected but got $actual")
      val sol = SolveContinuously.state.withValue(checker.csFactory.state.value) {
        expected.unify(actual.left.get, SolveContinuously.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestFJ(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val ev = e
      val actual = checker.typecheck(ev)

      val typ = ev.withType[checker.Result].typ._1
      val req = ev.withType[checker.Result].typ._2
      val creq = ev.withType[checker.Result].typ._3
      val cons = ev.withType[checker.Result].typ._4
      assert(actual.isLeft, s"Type = $typ, Reqs = $req, CReqs = $creq, Constraint = $cons")

    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }


  typecheckTestFJ("x", Var('x))(UCName(CVar('x)))
  typecheckTestFJ("e0.f : C ", Fields('f, Var('e0)))(UCName(CVar('C)))
  typecheckTestFJ("new C(x):C", New(Seq(CName('c)), Seq(Var('x))))(CName('c))
  typecheckTestFJ("e0.m() : void", Invk(Seq('m), Seq(Var('e0))))(UCName(CVar('void)))
  typecheckTestFJ("e0.m(e) : Double", Invk(Seq('m), Seq(Var('e0), Var('e))))(UCName(CVar('Double)))
  typecheckTestFJ("Point.add(p1, p2) : Point", Invk(Seq('add), Seq(Var('Point), Var('p1), Var('p2))))(UCName(CVar('Point)))
  typecheckTestError("(C) e0 :C", UCast(CName('c), Var('e)))
  typecheckTestError("y", Var('x))
  typecheckTestError("new Pair(first) : Pair", New(CName('Pair), Var('first)))
  typecheckTestError("new Pair(snd): Pair", New(CName('Pair), Var('object)))
  typecheckTestFJ("new Pair(first).setfst(first) : U ", Invk(Seq('setfst), Seq(New(Seq(CName('Pair)), Seq(Var('first))), Var('first))))(CName('int))
  typecheckTestError("(Object)first : Object", UCast(CName('Object), Var('first)))
  typecheckTestError("(Pair) first : Pair", New(CName('Pair), Var('first)))
  typecheckTestError("(Pair) first : Pair, second : Pair", New(CName('Pair), Var('first), Var('second)))
  typecheckTestFJ("new Object()", New(CName('Object)))(CName('Object))
  typecheckTestFJ("new Pair(fst : First, snd : Second)", New(Seq(CName('Pair)), Seq(Var('First), Var('Second))))(CName('Pair)) // see again why eq constraint does not work

  typecheckTestFJ(" (new Pair(first)).first : Int", Fields('f, New(CName('Pair), Var('f))))(UCName(CVar('Int)))


  typecheckTestFJ("'void m() ", MethodDec(Seq(CName('void), 'm, Seq(('x, UCName(CVar('x))), ('y, UCName(CVar('y))))), Seq(New(CName('Object)))))(CName('Object))
  typecheckTestFJ("'Double m( e0.m(x) ) in C", MethodDec(Seq(CName('void), 'm, Seq(('c, UCName(CVar('c))))), Seq(Invk(Seq('m), Seq(Var('e0), Var('c))))))(UCName(CVar('e0)))
  typecheckTestFJ("void m(f: p0) {New Pair first}", MethodDec(Seq(CName('void), 'm, Seq(('f, UCName(CVar('f))))), Seq(Invk(Seq('setfst), Seq(New(CName('Pair), Var('f)), Var('f))))))(CName('Pair))
  typecheckTestFJ("Pair first second", MethodDec(Seq(CName('Double), 'getSum, Seq(('fst, UCName(CVar('fst))))), Seq(Invk(Seq('add), Seq(Var('Point), Var('fst), Var('snd))))))(CName('Double))

  typecheckTestFJ("Adding two points", MethodDec(Seq(CName('Double), 'getSum, Seq(('fst, UCName(CVar('fst))), ('snd, UCName(CVar('snd))))), Seq(Invk(Seq('add), Seq(Var('Point), Var('fst), Var('snd))))))(CName('Point))

  //typecheckTestError("(C) e0 : C", DCast(CName('c),Var('e)))
  //typecheckTestError("(C) e0 : C", SCast(CName('c),'e))
  typecheckTestFJ("Int getX(x: Int) {return Int} ", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(New(CName('Int)))))(CName('Number))
  typecheckTestFJ("Int getX(x: Int) {Number.getX(x): Int}", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Invk(Seq('getX), Seq(Var('Number), Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Nr).getX(x): Int} ", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Invk(Seq('getX), Seq(New(CName('Nr)), Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number).gX(x): Int}", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Invk(Seq('gX), Seq(New(CName('Number)), Var('x))))))(CName('Number))

  typecheckTestFJ(" => Int getX(x: Int) {(new Number(x)).getX(x): Int} ", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Invk(Seq('getX), Seq(New(CName('Number)), Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int, y: Int) {(new Number).getX(x): Int}", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))), ('y, UCName(CVar('Int))))), Seq(Invk(Seq('getX), Seq(New(CName('Number)), Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number(x).x : Int in Number}", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Fields('x, New(CName('Number), Var('x))))))(CName('Number))

  typecheckTestFJ("Int getX(x: Int) {(new Number(x,y).x : Int in Number}", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))), Seq(Fields('x, New(CName('Number), Var('x), Var('y))))))(CName('Number))
  // /typecheckTestError("Int getXY(x,y) {return Int} in Number", Method(CName('Number), CName('Int), 'getXY, Seq('x, 'y),Var('e0)))


  typecheckTestFJ("e0.x + e0.x : TNum", Add(Fields('f, Num(1)), Fields('f, Num(1))))(TNum)
  typecheckTestFJ("e0.x + e0.x : C", Add(Fields('f, Num(1)), Fields('f, Num(1))))(CName('C))
  typecheckTestFJ("New Pair(new A(), new B()).snd", Fields('snd, New(Seq(CName('Pair)), Seq((New(CName('A))), (New(CName('B)))))))(CName('B))
  typecheckTestFJ("New Pair(new A(), 'snd).snd", Fields('snd, New(Seq(CName('Pair)), Seq((New(CName('A))), Var('snd)))))(CName('B))

  typecheckTestFJ("e0.m(x) + e0.m(x)", Add(Invk(Seq('m), Seq(Var('e0), Var('x))), Invk(Seq('m), Seq(Var('e0), Var('x)))))(TNum)
  typecheckTestFJ("x + x", Add(Var('x), Var('x)))(TNum)

  typecheckTestFJ("(New Pair(fst, snd)).setFirst(fst)", Invk(Seq('setFrist), Seq(New(Seq(CName('Pair)), Seq(Var('frt), Var('snd))), Var('frt))))(TNum)

  typecheckTestFJ(" Pair Int First ", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('Int)))), Seq()))(CName('Pair))

  typecheckTestFJ(" ========> Int getX(){ New(Pair(Var(x))).x } ",
    ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('Int)))),
      Seq(MethodDec(Seq(CName('Int), 'getX, Seq()), Seq(Fields('x, New(CName('Pair), Var('x))))))))(CName('Pair))

  typecheckTestFJ(" Pair Int First, Int {(new Number(x)).getX(x): Int} ", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('Int)))), Seq(MethodDec(Seq(CName('Int), 'getX, Seq(('x, CName('Int)))), Seq(New(CName('Int)))))))(CName('Pair))

  typecheckTestFJ(" Pair String First, Double getX(First : Double){ return (New Pair).First} ", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('String)))), Seq(MethodDec(Seq(CName('Double), 'getX, Seq()), Seq(Fields('First, New(CName('Pair), Var('First))))))))(CName('Pair))

  typecheckTestFJ(" Pair Double First, Double getX(First : Double){ return (New Pair).First}  ", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('Double)))), Seq(MethodDec(Seq(CName('Double), 'getX, Seq()), Seq(Fields('First, New(CName('Pair), Var('First))))))))(CName('Pair))

  typecheckTestFJ(" Object Int setFirst(Int First) ", ClassDec(Seq(CName('Object), CName('Object), Seq()), Seq(MethodDec(Seq(CName('Object), 'setFirst, Seq(('First, CName('Int)))), Seq(New(CName('Object)))))))(CName('Object))

  typecheckTestFJ(" Pair Int First, Int Second,setFirst(Int First) ", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('Int)), ('Second, CName('Int)))), Seq(MethodDec(Seq(CName('Void), 'setFirst, Seq(('First, CName('Int)))), Seq(New(CName('Void)))))))(CName('Pair))

  typecheckTestFJ(" Pair Int First, Int Second,  setSecond(Int Second) ", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('Int)), ('Second, CName('Int)))), Seq(MethodDec(Seq(CName('Void), 'setFirst, Seq(('Second, CName('Int)))), Seq(New(CName('Void)))))))(CName('Pair))

  typecheckTestFJ(" Pair Int First, Int Second, Int sum(Int First, Int Second) ", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('Int)), ('Second, CName('Int)))), Seq(MethodDec(Seq(CName('Int), 'sum, Seq(('First, CName('Int)), ('Second, CName('Int)))), Seq(New(CName('Sum)))))))(CName('Pair))

  typecheckTestFJ("Pair Int First, Int Second, Int add(first, second) { return (New Pair).add(first, second) }", ClassDec(Seq(CName('Pair), CName('Object), Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(MethodDec(Seq(CName('TNum), 'add, Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(Add(Fields('First, New(CName('Pair), Var('First))), Fields('Second, New(CName('Pair), Var('Second)))))))))(CName('Pair))
  //see another time this example and see how I could change the other examples

  typecheckTestFJ("Pair String First, Int Second, Int add(first, second) { return (New Pair).add(first String, second TNum) }", ClassDec(Seq(CName('TNum), CName('Object), Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(MethodDec(Seq(CName('TNum), 'add, Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(Add(Fields('First, New(CName('String))), Fields('Second, New(CName('TNum)))))))))(CName('TNum))

  typecheckTestFJ("Pair Int First, Int Second, Int add(first Int, second Int) { return add(first String, second Int) }", ClassDec(Seq(CName('TNum), CName('Object), Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(MethodDec(Seq(CName('String), 'add, Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(Add(Fields('First, New(CName('TNum))), Fields('Second, New(CName('TNum)))))))))(CName('TNum))

  typecheckTestFJ("Pair String First, Int Second, Int add(first Int, second Int) { return add(first, second) }", ClassDec(Seq(CName('TNum), CName('Object), Seq(('First, CName('String)), ('Second, CName('TNum)))), Seq(MethodDec(Seq(CName('TNum), 'add, Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(Add(Fields('First, New(CName('TNum))), Fields('Second, New(CName('TNum)))))))))(CName('TNum))

  typecheckTestFJ("Pair Int First, Int Second, Int add(first String, second Int) { return add(first : String, second : TNum) }", ClassDec(Seq(CName('TNum), CName('Object), Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(MethodDec(Seq(CName('TNum), 'add, Seq(('First, CName('TNum)), ('Second, CName('TNum)))), Seq(Add(Fields('First, New(CName('String))), Fields('Second, New(CName('TNum)))))))))(CName('TNum))

  typecheckTestFJ("Pair Int x, Int getX(){ New(Pair(Int).x)}}",
    ClassDec(Seq(CName('Pair), CName('Object), Seq(('x, CName('Int)))),
      Seq(MethodDec(Seq(CName('Int), 'getX, Seq()), Seq(Fields('x, New(CName('Pair), Var('x))))))))(CName('Pair))

  typecheckTestFJ("Class C, Int foo(){return 1+1}} ", ClassDec(Seq(CName('C), CName('Object), Seq()), Seq(MethodDec(Seq(CName('TNum), 'foo, Seq()), Seq(Add(Num(1), Num(1)))))))(CName('C))

  typecheckTestFJ("String foo(){return 1+1}", MethodDec(Seq(CName('String), 'foo, Seq()), Seq(Add(Num(1), Num(1)))))(CName('C))

  typecheckTestFJ("Int foo(){return 1+1}", MethodDec(Seq(CName('TNum), 'foo, Seq()), Seq(Add(Num(1), Num(1)))))(CName('C))

  typecheckTestFJ("Class C, Int foo(){return 1+1}, String bar(){return foo();} ", ClassDec(Seq(CName('C), CName('Object), Seq()),
    Seq(MethodDec(Seq(CName('TNum), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))))(CName('C))

  typecheckTestFJ("Class C, Tnum foo(){return 1+1}, TNum bar(){return foo();} ", ClassDec(Seq(CName('C), CName('Object), Seq()),
    Seq(MethodDec(Seq(CName('TNum), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('TNum), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))))(CName('C))

  typecheckTestFJ("string c.foo", MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))(CName('C))

  //typecheckTestFJ("Int m(x Int){return x} in C, C.m(x Int)", ProgramM(Seq(CName('Number)), Seq(MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(New(CName('Int)))), Invk(Seq('m), Seq(Var('e0), Var('x))))))(CName('Object))

  // typecheckTestFJ("Int getX(x Int){return x} in Number, Int Number.get(x Int)", ProgramM(Seq(), Seq(MethodDec(Seq(CName('Number), CName('Int),'getX, Seq(('x, UCName(CVar('Int))))),Seq(New(CName('Int)))), Invk(Seq('getX), Seq(New(CName('Number)), Var('x))))))(CName('Number))

  // typecheckTestFJ("Int ")

}

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))

