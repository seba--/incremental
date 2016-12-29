package incremental.fjava

import constraints.CVar
import constraints.fjava._
import constraints.fjava.impl._
import incremental.Node._
import incremental.fjava.latemerge.{BUChecker, BUCheckerFactory}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

import scala.collection.immutable.ListMap

/**
 * Created by lirakuci on 3/29/15.
 */
class TestCorrectness[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  val nats = new Nats
  val strings = new Strings
  import nats._
  import strings._

  def typecheckTestFJ(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val ev = if (e.kind != ProgramM) e else ProgramM(Seq(), e.kids.seq ++ nats.all :+ strings.string)
      val actual = checker.typecheck(ev)

//      val typ = ev.withType[checker.Result].typ._1
//      val req = ev.withType[checker.Result].typ._2
//      val creq = ev.withType[checker.Result].typ._3
//      val cons = ev.withType[checker.Result].typ._4
      assert(actual.isLeft, actual.right)

      val sol = SolveContinuousSubst.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuousSubst.freshConstraintSystem).tryFinalize      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTestError("e0.f : C ", FieldAcc('f, Var('e0))) //(UCName(CVar('C)))
  typecheckTestError("new C(x):C", New(Seq(CName('c)), Seq(Var('x)))) //(CName('c))
  typecheckTestError("e0.m() : void", Invk(Seq('m), Seq(Var('e0)))) //(UCName(CVar('void)))
  typecheckTestError("e0.m(e) : Double", Invk(Seq('m), Seq(Var('e0), Var('e))))//(UCName(CVar('Double)))
  typecheckTestError("Point.add(p1, p2) : Point", Invk(Seq('add), Seq(Var('Point), Var('p1), Var('p2)))) //(UCName(CVar('Point)))
  typecheckTestError("(C) e0 :C", UCast(CName('c), Var('e)))

  typecheckTestError("new Pair(first) : Pair", New(CName('Pair), Var('first)))
  typecheckTestError("new Pair(snd): Pair", New(CName('Pair), Var('object)))
  typecheckTestError("new Pair(first).setfst(first) : U ", Invk(Seq('setfst), Seq(New(Seq(CName('Pair)), Seq(Var('first))), Var('first))))//(CName('int))
  typecheckTestError("(Object)first : Object", UCast(CName('Object), Var('first)))
  typecheckTestError("(Pair) first : Pair", New(CName('Pair), Var('first)))
  typecheckTestError("(Pair) first : Pair, second : Pair", New(CName('Pair), Var('first), Var('second)))
  typecheckTestFJ("new Object()", New(CName('Object)))(CName('Object)) //created object class with empty fields and methods, Object is the encoded Bottom Type
  typecheckTestError("new Pair(fst : First, snd : Second)", New(Seq(CName('Pair)), Seq(Var('First), Var('Second))))//(CName('Pair)) // see again why eq constraint does not work

  typecheckTestError(" (new Pair(first)).first : Int", FieldAcc('f, New(CName('Pair), Var('f)))) //(UCName(CVar('Int)))


  typecheckTestError("'void m(x, y) ", MethodDec(Seq(CName('void), 'm, Seq(('x, UCName(CVar('x))), ('y, UCName(CVar('y))))),
    Seq(New(CName('Object)))))//(CName('Object))
  typecheckTestError("'Double m( e0.m(x) ) in C", MethodDec(Seq(CName('void), 'm, Seq(('c, UCName(CVar('c))))),
    Seq(Invk(Seq('m), Seq(Var('e0), Var('c)))))) // recoursive call, so it is correct
  typecheckTestError("void m() {New Pair first}", MethodDec(Seq(CName('void), 'm, Seq()), Seq(Invk(Seq('setfst),
    Seq(New(CName('Pair), Var('first)), Var('first))))))//(CName('Pair))
  typecheckTestError("Pair first second", MethodDec(Seq(CName('Double), 'getSum, Seq()), Seq(Invk(Seq('add),
    Seq(Var('Point), Var('fst), Var('snd))))))//(CName('Double))

  //typecheckTestFJ("Tnum add(first, second) {first + seconds}", MethodDec(Seq(CName('Nat), 'getSum, Seq(('fst, UCName(CVar('fst))), ('snd, UCName(CVar('snd))))), Seq(Invk(Seq('add), Seq(Var('Point), Var('fst), Var('snd))))))(CName('Point))

  typecheckTestError("String add(first, second) {first + seconds}", MethodDec(Seq(CName('String), 'getSum, Seq(('fst, CName('Nat)), ('snd, CName('Nat)))),
    Seq(Add(Num(1), Num(2)))))//(CName('Nat))

 // typecheckTestFJ("TNum add(first, second) {first + seconds}", MethodDec(Seq(CName('Nat), 'getSum, Seq(('fst, CName('Nat)), ('snd, CName('Nat)))),
   // Seq(Add(Num(1), Num(2)))))(CName('Nat))

  //typecheckTestError("(C) e0 : C", DCast(CName('c),Var('e)))
  //typecheckTestError("(C) e0 : C", SCast(CName('c),'e))

//  typecheckTestFJ("Int getX(x: Int) {return Int} ", MethodDec(Seq(CName('Nat), 'getX, Seq(('x, CName('Nat)))),
   // Seq(Num(0))))(CName('Number))

//  typecheckTestFJ("Int getX(x: Int) {Number.getX(x): Int}", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))),
    //Seq(Invk(Seq('getX), Seq(New(CName('Number)), Num(1))))))(CName('Number))

//  typecheckTestFJ("Int getX(x: Int) {(new Nr).getX(x): Int} ", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))),
    //Seq(Invk(Seq('getX), Seq(New(CName('Nr)), Var('x))))))(CName('Nr))

  typecheckTestError("Int getX(x: Int) {(new Number).gX(x): Int}", MethodDec(Seq(CName('Int), 'getX, Seq(('x, UCName(CVar('Int))))),
    Seq(Invk(Seq('gX), Seq(New(CName('Number)), Var('x))))))//(CName('Number))

  typecheckTestError("Int getX(x: Int) {(new Number(x)).getX(x): Int} ", MethodDec(Seq(CName('Int), 'getX, Seq()),
    Seq(Invk(Seq('getX), Seq(New(CName('Number)), Var('x))))))//(CName('Number))

  typecheckTestError("Int getX(x: Int) {(new Number(x).x : Int in Number}", MethodDec(Seq(CName('Int), 'getX, Seq()),
    Seq(FieldAcc('x, New(CName('Number), Var('x))))))//(CName('Number))

  typecheckTestError("Int getX(x: Int) {(new Number(x,y).x : Int in Number}", MethodDec(Seq(CName('Int), 'getX, Seq()),
    Seq(FieldAcc('x, New(CName('Number), Var('x), Var('y))))))//(ßCName('Number))
  // /typecheckTestError("Int getXY(x,y) {return Int} in Number", Method(CName('Number), CName('Int), 'getXY, Seq('x, 'y),Var('e0)))


  typecheckTestError("e0.x + e0.x : TNum", Add(FieldAcc('f, Num(1)), FieldAcc('f, Num(1))))//(TNum)
  typecheckTestError("e0.x + e0.x : C", Add(FieldAcc('f, Num(1)), FieldAcc('f, Num(1))))//(CName('C))
  typecheckTestError("New Pair(new A(), new B()).snd", FieldAcc('snd, New(Seq(CName('Pair)), Seq((New(CName('A))), (New(CName('B)))))))//(CName('B))
  typecheckTestError("New Pair(new A(), 'snd).snd", FieldAcc('snd, New(Seq(CName('Pair)), Seq((New(CName('A))), Var('snd)))))//(CName('B))

  typecheckTestError("e0.m(x) + e0.m(x)", Add(Invk(Seq('m), Seq(Var('e0), Var('x))), Invk(Seq('m), Seq(Var('e0), Var('x)))))//(TNum)
  typecheckTestError("x + x", Add(Var('x), Var('x)))

  typecheckTestError("(New Pair(fst, snd)).setFirst(fst)", Invk(Seq('setFrist), Seq(New(Seq(CName('Pair)), Seq(Var('frt), Var('snd))), Var('frt))))//(TNum)

  val Fst =  ClassDec(Seq(CName('Pair), CName('Object), Ctor(ListMap(), ListMap('Pair -> CName('Int))), Seq(('First, CName('Int)))), Seq())
  typecheckTestFJ(" Pair Int First ",ProgramM(Fst))(ProgramOK)

  val GetX = ClassDec(Seq(CName('Pair), CName('Object), Ctor(ListMap(), ListMap('First -> CName('Int))), Seq(('First, CName('Int)))),
    Seq(MethodDec(Seq(CName('Nat), 'getX, Seq(('x, CName('Int)))), Seq(Num(0)))))
  typecheckTestFJ(" Pair Int First, Int {(new Number(x)).getX(x): Int} ", ProgramM(GetX))(ProgramOK)
  typecheckTestError(" Pair String First, Double getX(First : Double){ return (New Pair).First} ", ClassDec(Seq(CName('Pair), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('String))), Seq(('First, CName('String)))),
    Seq(MethodDec(Seq(CName('Double), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair), Var('First))))))))//(CName('Pair))


  val Pair2 = ClassDec(Seq(CName('Pair), CName('Object), Ctor(ListMap(), ListMap('First -> CName('Nat))),
    Seq(('First, CName('Nat)))),
    Seq(MethodDec(Seq(CName('Nat), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair), Num(1)))))))

  typecheckTestFJ(" ========>>>> Pair Double First, Double getX(){ return (New Pair(Num(1))).First}  ", ProgramM(Pair2) )(ProgramOK) // look again at the combination of these examples


  val Pair1 =  ClassDec(Seq(CName('Pair), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))),
    Seq(MethodDec(Seq(CName('Nat), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair), Num(1), Num(2)))))))
  typecheckTestFJ("Pair Tnum First, Tnum Second, TNum getX() {return (New Pair(Num(1), Num(2)).First)}",ProgramM(Pair1))(ProgramOK)

  typecheckTestError("Pair TNum first, TNum second, String getX() {return (New PAir(Num(1), Num(2)).first)}", ClassDec(Seq(CName('Pair), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))),
  Seq(MethodDec(Seq(CName('String), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair), Num(0), Num(1))))))))

  typecheckTestError("Pair TNum first, string second, TNum getX() {return (New PAir(Num(1), Num(2)).first)}", ClassDec(Seq(CName('Pair), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('String))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))),
    Seq(MethodDec(Seq(CName('String), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair), Num(0), Num(1))))))))

  val Pair = ClassDec(Seq(CName('Pair), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))),
    Seq(MethodDec(Seq(CName('Nat), 'add, Seq()),
          Seq(Add(FieldAcc('First, New(CName('Pair), Num(1), Num(2))), FieldAcc('Second, New(CName('Pair), Num(1), Num(2))))))))
  typecheckTestFJ("Pair Int First, Int Second, Int add() { return (New Pair).add(first, second) }", ProgramM(Pair) )(ProgramOK)

  typecheckTestError("Pair Int First, Int Second, String addField() { return add(first Int, second Int) }", ClassDec(Seq(CName('Nat), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))), Seq(MethodDec(Seq(CName('String), 'add, Seq()),
    Seq(Add(FieldAcc('First, New(CName('Pair), Num(0))), FieldAcc('Second, New(CName('Pair), Num(1)))))))))//(CName('Nat))

  typecheckTestError("Pair String First, Int Second, Int addField() { return add(first, second) }", ClassDec(Seq(CName('Nat), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
    Seq(('First, CName('String)), ('Second, CName('Nat)))), Seq(MethodDec(Seq(CName('Nat), 'add, Seq()),
    Seq(Add(FieldAcc('First, New(CName('Pair), Num(0))), FieldAcc('Second, New(CName('Pair), Num(1))))))))) //(CName('Nat))

  typecheckTestError("Pair Int First, Int Second, Int addField() { return add(first : String, second : TNum) }", ClassDec(Seq(CName('Nat), CName('Object),
    Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))), Seq(MethodDec(Seq(CName('Nat), 'add, Seq()),
    Seq(Add(FieldAcc('First, New(CName('Pair), Str('a))), FieldAcc('Second, New(CName('Pair), Num(0)))))))))//(CName('Nat))

 val foo =  ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
   Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1))))))
  typecheckTestFJ("Class C, Int foo(){return 1+1}} ",ProgramM(foo))(ProgramOK)

  typecheckTestError("String foo(){return 1+1}", MethodDec(Seq(CName('String), 'foo, Seq()), Seq(Add(Num(1), Num(1)))))//(CName('C))

 // typecheckTestFJ("Int foo(){return 1+1}", MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))))(CName('C))
  typecheckTestError("string c.foo", MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))//(CName('C))

  typecheckTestError("Class C, Int foo(){return 1+1}, String bar(){return foo();} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))))//(CName('C))

  val C0 = ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C))))))))
    typecheckTestFJ("Class C, Tnum foo(){return 1+1}, TNum bar(){return foo();} ", ProgramM(C0))(ProgramOK)

  typecheckTestError("Class C, Tnum foo(x TNum){return 1+1}, TNum bar(){return foo();} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))))

  typecheckTestError("Class C, Tnum foo(x int){return 1+1}, TNum bar(){return foo('a);} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this), Str('a))))))))

  val C1 = ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)),Num(0)))))))

  typecheckTestFJ("Class C, Tnum foo(x int){return 1+1}, TNum bar(){return foo(Num(1));} ", ProgramM(C1))(ProgramOK)

  typecheckTestError("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Num(1));} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Num(1))))))))

  typecheckTestError("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Num(1), string a);} ", ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Num(1), Str('a))))))))

  typecheckTestError("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Var(x), Num(2));} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Var('x), Num(2))))))))

   val C2 = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
     Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
       MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Num(1), Num(2)))))))

  typecheckTestFJ("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Num(1), Num(2));} ", ProgramM(C2))(ProgramOK)

  val C3 =  ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this))))),
      MethodDec(Seq(CName('Nat), 'getbar, Seq()), Seq(Invk(Seq('bar), Seq(Var('this)))))))
  typecheckTestFJ("Class C, Tnum foo(){return 1+1}, TNum bar(){return foo()}, TNum getBar() {return bar} ", ProgramM(C3))(ProgramOK)

  typecheckTestError("Class C, Tnum foo(){return 1+1}, TNum bar(){return foo()}, String getBar() {return bar} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))),
      MethodDec(Seq(CName('String), 'getbar, Seq()), Seq(Invk(Seq('bar), Seq(New(CName('C)))))))))//(CName('C))

  typecheckTestError("Class C, TNum add(a Int, b Int){ C.add('a 2)}", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'add, Seq(('a, CName('Nat)), ('b, CName('Nat)))), Seq(Invk(Seq('add), Seq(New(CName('C)), Str('a), Num(2))))))))


  typecheckTestFJ("Class C, Tnum foo(x int, y int){return x+y}} ", ProgramM(ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Var('x), Var('y))))))))(ProgramOK)

  typecheckTestError("Class C, Tnum foo(x int, y int){return z+z}} ", ProgramM(ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Var('z), Var('z))))))))

  val M1 =  ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Num(1)))))

  val M2 =  ClassDec(Seq(CName('C1), CName('C),Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val M3 =  ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('String), 'foo, Seq()), Seq(Str('a)))))

  val M4 =  ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Num(1)))))

  val M5 =  ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val M6 =  ClassDec(Seq(CName('C3), CName('C2),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Num(1)))))

  typecheckTestError("{M1, M2, M3} ok", ProgramM(M1, M2, M3))
  typecheckTestFJ("{M1, M2, M4} ok", ProgramM(M1, M2, M4))(ProgramOK)
  typecheckTestFJ("{M1, M2, M5, M6} ok", ProgramM(M1, M2, M5, M6))(ProgramOK)
  typecheckTestError("{M1, M2, M3, M6} ok", ProgramM(M1, M2, M3, M6))

  val MutualRec = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Invk(Seq('bar), Seq(Var('this))))),
    MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this)))))))

  val WMutualRec = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Invk(Seq('bar), Seq(Var('this))))),
      MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this)))))))

  typecheckTestFJ("{C int foo() {this.bar()}, int bar() {this.foo()}} ok", ProgramM(MutualRec))(ProgramOK)
  typecheckTestError("{C string foo() {this.bar()}, int bar() {this.foo()}}ok", ProgramM(WMutualRec))

  val mutS1 = ClassDec(Seq(CName('C1), CName('C2),Ctor(ListMap(), ListMap()), Seq()),
  Seq())

  val mutS2 = ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  typecheckTestError("{Mutual Super Types} ok", ProgramM(mutS1, mutS2))

  val ctorFC = ClassDec(Seq(CName('Pair), CName('Object),  Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))), Seq())

  val wctorFC = ClassDec(Seq(CName('Pair), CName('Object),  Ctor(ListMap(), ListMap('First -> CName('Nat))),
    Seq(('First, CName('Nat)), ('Second, CName('Nat)))), Seq())

  typecheckTestFJ("{ctorFC} ok", ProgramM(ctorFC))(ProgramOK)
  typecheckTestError("{wctorFC} ok", ProgramM(wctorFC))

  val A = ClassDec(Seq(CName('A), CName('Object),Ctor(ListMap(), ListMap('i -> CName('Nat))), Seq(('i, CName('Nat)))),
    Seq(MethodDec(Seq(CName('Object), 'm, Seq(('b, CName('B)))), Seq(New(CName('B), FieldAcc('i, Var('this)))))))

  val B = ClassDec(Seq(CName('B), CName('A), Ctor(ListMap('i -> CName('Nat)), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Object), 'm, Seq(('b, CName('B)))), Seq(FieldAcc('i, UCast(CName('A),Var('b)))))))

  typecheckTestFJ("{A, B} ok", ProgramM(A, B))(ProgramOK)

  val ASup = ClassDec(Seq(CName('A), CName('Object),Ctor(ListMap(), ListMap()), Seq()), Seq())

  val BSub = ClassDec(Seq(CName('B), CName('A), Ctor(ListMap(), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Object), 'm, Seq()), Seq(FieldAcc('i, Var('this))))))

  typecheckTestError("{ASup, BSub} not ok: unprovided field 'i'", ProgramM(ASup, BSub))

  val AS = ClassDec(Seq(CName('A), CName('Object), Ctor(ListMap('i -> CName('Nat)), ListMap()), Seq()),
    Seq(MethodDec(Seq(CName('Object), 'm, Seq()), Seq(FieldAcc('i, Var('this))))))

  val BS = ClassDec(Seq(CName('B), CName('Object), Ctor(ListMap(), ListMap('i -> CName('Nat))), Seq(('i, CName('Nat)))),
    Seq(MethodDec(Seq(CName('Object), 'm, Seq()), Seq(FieldAcc('i, Var('this))))))

 // val CS = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()), Seq())

  typecheckTestFJ("{ASup, CSub}", ProgramM(BS))(ProgramOK)


}

class TestDUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new DUCheckerFactory(SolveEnd))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))

//class TestBUEarlySolveContinuousSubstCorrectness extends TestCorrectness("BUEarlySolveContinuousSubst", new earlymerge.BUCheckerFactory(SolveContinuousSubst))
