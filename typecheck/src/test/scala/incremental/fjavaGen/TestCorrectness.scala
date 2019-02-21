package incremental.fjavaGen

import constraints.fjavaGen._
import constraints.fjavaGen.impl._
import incremental.Node._
import incremental.fjavaGen.latemerge.BUCheckerFactory
import org.scalatest.{BeforeAndAfterEach, FunSuite}

import scala.collection.immutable.ListMap

/**
 * Created by lirakuci on 3/29/15.
 */
class TestCorrectness[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()


  def typecheckTestFJ(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val ev = if (e.kind != ProgramM) e else ProgramM(Seq(), e.kids.seq)
      val actual = checker.typecheck(ev)

//      val typ = ev.withType[checker.Result].typ._1
//      val req = ev.withType[checker.Result].typ._2
//      val creq = ev.withType[checker.Result].typ._3
//      val cons = ev.withType[checker.Result].typ._4
      assert(actual.isLeft, actual.right)

      val sol = SolveContinuousSubstLateMerge.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuousSubstLateMerge.freshConstraintSystem).tryFinalize      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

//  typecheckTestError("e0.f : C ", FieldAcc('f, Var('e0))) //(UCName(CVar('C)))
//  typecheckTestError("new C(x):C", New(Seq(CName('c, Seq())), Seq(Var('x)))) //(CName('c))
//  typecheckTestError("e0.m() : void", Invk(Seq('m), Seq(Var('e0)))) //(UCName(CVar('void)))
//  typecheckTestError("e0.m(e) : Double", Invk(Seq('m), Seq(Var('e0), Var('e))))//(UCName(CVar('Double)))
//  typecheckTestError("Point.add(p1, p2) : Point", Invk(Seq('add), Seq(Var('Point), Var('p1), Var('p2)))) //(UCName(CVar('Point)))
//  typecheckTestError("(C) e0 :C", UCast(CName('c, Seq()), Var('e)))
//
//  typecheckTestError("new Pair(first) : Pair", New(CName('Pair, Seq()), Var('first)))
//  typecheckTestError("new Pair(snd): Pair", New(CName('Pair, Seq()), Var('object)))
//  typecheckTestError("new Pair(first).setfst(first) : U ", Invk(Seq('setfst), Seq(New(Seq(CName('Pair, Seq())), Seq(Var('first))), Var('first))))//(CName('int))
//  typecheckTestError("(Object)first : Object", UCast(CName('Object, Seq()), Var('first)))
//  typecheckTestError("(Pair) first : Pair", New(CName('Pair, Seq()), Var('first)))
//  typecheckTestError("(Pair) first : Pair, second : Pair", New(CName('Pair, Seq()), Var('first), Var('second)))
//  typecheckTestFJ("new Object()", New(CName('Object, Seq())))(CName('Object, Seq())) //created object class with empty fields and methods, Object is the encoded Bottom Type
//  typecheckTestError("new Pair(fst : First, snd : Second)", New(Seq(CName('Pair, Seq())), Seq(Var('First), Var('Second))))//(CName('Pair)) // see again why eq constraint does not work
//
//  typecheckTestError(" (new Pair(first)).first : Int", FieldAcc('f, New(CName('Pair, Seq()), Var('f)))) //(UCName(CVar('Int)))
//
//
//  typecheckTestError("'void m(x, y) ", MethodDec(Seq(CName('void, Seq()), 'm, Seq(('x, CName('Tx, Seq())), ('y, CName('Ty, Seq())))),
//    Seq(New(CName('Object, Seq())))))//(CName('Object))
//  typecheckTestError("'Double m( e0.m(x) ) in C", MethodDec(Seq(CName('void, Seq()), 'm, Seq(('c, CName('Tc, Seq())))),
//    Seq(Invk(Seq('m), Seq(Var('e0), Var('c)))))) // recoursive call, so it is correct
//  typecheckTestError("void m() {New Pair first}", MethodDec(Seq(CName('void, Seq()), 'm, Seq()), Seq(Invk(Seq('setfst),
//    Seq(New(CName('Pair, Seq()), Var('first)), Var('first))))))//(CName('Pair))
//  typecheckTestError("Pair first second", MethodDec(Seq(CName('Double, Seq()), 'getSum, Seq()), Seq(Invk(Seq('add),
//    Seq(Var('Point), Var('fst), Var('snd))))))//(CName('Double))
//
//  //typecheckTestFJ("Tnum add(first, second) {first + seconds}", MethodDec(Seq(CName('Nat), 'getSum, Seq(('fst, UCName(CVar('fst))), ('snd, UCName(CVar('snd))))), Seq(Invk(Seq('add), Seq(Var('Point), Var('fst), Var('snd))))))(CName('Point))
//
//  typecheckTestError("String add(first, second) {first + seconds}", MethodDec(Seq(CName('String, Seq()), 'getSum, Seq(('fst, CName('Nat, Seq())), ('snd, CName('Nat, Seq())))),
//    Seq(Add(Num(1), Num(2)))))//(CName('Nat))
//
//
//  typecheckTestError("(New Pair(fst, snd)).setFirst(fst)", Invk(Seq('setFrist), Seq(New(Seq(CName('Pair, Seq())), Seq(Var('frt), Var('snd))), Var('frt))))//(TNum)
//
//  val Fst =  ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()), Ctor(ListMap(), ListMap('Pair -> CName('Int, Seq()))), Seq(('First, CName('Int, Seq())))), Seq())
//  typecheckTestFJ(" Pair Int First ",ProgramM(Fst))(ProgramOK)
//
//  val GetX = ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()), Ctor(ListMap(), ListMap('First -> CName('Int, Seq()))), Seq(('First, CName('Int, Seq())))),
//    Seq(MethodDec(Seq(CName('Nat, Seq()), 'getX, Seq(('x, CName('Int, Seq())))), Seq(Num(0)))))
//  typecheckTestFJ(" Pair Int First, Int {(new Number(x)).getX(x): Int} ", ProgramM(GetX))(ProgramOK)
//  typecheckTestError(" Pair String First, Double getX(First : Double){ return (New Pair).First} ", ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()),
//    Ctor(ListMap(), ListMap('First -> CName('String, Seq()))), Seq(('First, CName('String, Seq())))),
//    Seq(MethodDec(Seq(CName('Double, Seq()), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair, Seq()), Var('First))))))))//(CName('Pair))
//
//
//  val Pair2 = ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()), Ctor(ListMap(), ListMap('First -> CName('Nat, Seq()))),
//    Seq(('First, CName('Nat, Seq())))),
//    Seq(MethodDec(Seq(CName('Nat, Seq()), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair, Seq()), Num(1)))))))
//
//  typecheckTestFJ(" ========>>>> Pair Double First, Double getX(){ return (New Pair(Num(1))).First}  ", ProgramM(Pair2) )(ProgramOK) // look again at the combination of these examples
//
//
//  val Pair1 =  ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()),
//    Ctor(ListMap(), ListMap('First -> CName('Nat, Seq()), 'Second -> CName('Nat, Seq()))),
//    Seq(('First, CName('Nat, Seq())), ('Second, CName('Nat, Seq())))),
//    Seq(MethodDec(Seq(CName('Nat, Seq()), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair, Seq()), Num(1), Num(2)))))))
//  typecheckTestFJ("Pair Tnum First, Tnum Second, TNum getX() {return (New Pair(Num(1), Num(2)).First)}",ProgramM(Pair1))(ProgramOK)
//
//  typecheckTestError("Pair TNum first, TNum second, String getX() {return (New PAir(Num(1), Num(2)).first)}", ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()),
//    Ctor(ListMap(), ListMap('First -> CName('Nat, Seq()), 'Second -> CName('Nat, Seq()))),
//    Seq(('First, CName('Nat, Seq())), ('Second, CName('Nat, Seq())))),
//  Seq(MethodDec(Seq(CName('String, Seq()), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair, Seq()), Num(0), Num(1))))))))
//
//  typecheckTestError("Pair TNum first, string second, TNum getX() {return (New PAir(Num(1), Num(2)).first)}", ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()),
//    Ctor(ListMap(), ListMap('First -> CName('Nat, Seq()), 'Second -> CName('String, Seq()))),
//    Seq(('First, CName('Nat, Seq())), ('Second, CName('Nat, Seq())))),
//    Seq(MethodDec(Seq(CName('String, Seq()), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair, Seq()), Num(0), Num(1))))))))
//
//  val Pair = ClassDec(Seq(CName('Pair, Seq()), CName('Object, Seq()),
//    Ctor(ListMap(), ListMap('First -> CName('Nat, Seq()), 'Second -> CName('Nat, Seq()))),
//    Seq(('First, CName('Nat, Seq())), ('Second, CName('Nat, Seq())))),
//    Seq(MethodDec(Seq(CName('Nat, Seq()), 'add, Seq()),
//          Seq(Add(FieldAcc('First, New(CName('Pair, Seq()), Num(1), Num(2))), FieldAcc('Second, New(CName('Pair, Seq()), Num(1), Num(2))))))))
//  typecheckTestFJ("Pair Int First, Int Second, Int add() { return (New Pair).add(first, second) }", ProgramM(Pair) )(ProgramOK)
//
//  typecheckTestError("Pair Int First, Int Second, String addField() { return add(first Int, second Int) }", ClassDec(Seq(CName('Nat, Seq()), CName('Object, Seq()),
//    Ctor(ListMap(), ListMap('First -> CName('Nat, Seq()), 'Second -> CName('Nat, Seq()))),
//    Seq(('First, CName('Nat, Seq())), ('Second, CName('Nat, Seq())))), Seq(MethodDec(Seq(CName('String, Seq()), 'add, Seq()),
//    Seq(Add(FieldAcc('First, New(CName('Pair, Seq()), Num(0))), FieldAcc('Second, New(CName('Pair, Seq()), Num(1)))))))))//(CName('Nat))
//
//  typecheckTestError("Pair String First, Int Second, Int addField() { return add(first, second) }", ClassDec(Seq(CName('Nat, Seq()), CName('Object, Seq()),
//    Ctor(ListMap(), ListMap('First -> CName('Nat, Seq()), 'Second -> CName('Nat, Seq()))),
//    Seq(('First, CName('String, Seq())), ('Second, CName('Nat, Seq())))), Seq(MethodDec(Seq(CName('Nat, Seq()), 'add, Seq()),
//    Seq(Add(FieldAcc('First, New(CName('Pair, Seq()), Num(0))), FieldAcc('Second, New(CName('Pair, Seq()), Num(1))))))))) //(CName('Nat))

  typecheckTestFJ("Pair<X, Y> first X, second Y, Num getFirst()  new Pair<Num, Num>(Num(1), Num(2))", ProgramM(ClassDec(Seq(CName('Pair, Seq(TVar('X), TVar('Y))), Seq(CName('Object, Seq()), CName('Object, Seq())), CName('Object, Seq()),
    Ctor(ListMap(), ListMap('First -> TVar('X), 'Second -> TVar('Y))),
    Seq(('First, TVar('X)), ('Second, TVar('Y)))),
    Seq(MethodDec(Seq(CName('Num, Seq()), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair, Seq(CName('Num, Seq()), CName('Num, Seq()))), Num(1), Num(2)))))))))(ProgramOK)

  typecheckTestError("Pair<X, Y> first X, second Y, Num getFirst()  new Pair<String, Num>(Num(1), Num(2))", ProgramM(ClassDec(Seq(CName('Pair, Seq(TVar('X), TVar('Y))), Seq(CName('Object, Seq()), CName('Object, Seq())),CName('Object, Seq()),
    Ctor(ListMap(), ListMap('First -> TVar('X), 'Second -> TVar('Y))),
    Seq(('First, TVar('X)), ('Second, TVar('Y)))),
    Seq(MethodDec(Seq(CName('Num, Seq()), 'getX, Seq()), Seq(FieldAcc('First, New(CName('Pair, Seq(CName('Sting, Seq()), CName('Num, Seq()))), Num(1), Num(2)))))))))


  // val foo =  ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
//   Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1))))))
//  typecheckTestFJ("Class C, Int foo(){return 1+1}} ",ProgramM(foo))(ProgramOK)
//
//  typecheckTestError("String foo(){return 1+1}", MethodDec(Seq(CName('String), 'foo, Seq()), Seq(Add(Num(1), Num(1)))))//(CName('C))
//
// // typecheckTestFJ("Int foo(){return 1+1}", MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))))(CName('C))
//  typecheckTestError("string c.foo", MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))//(CName('C))
//
//  typecheckTestError("Class C, Int foo(){return 1+1}, String bar(){return foo();} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))))//(CName('C))
//
//  val C0 = ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C))))))))
//    typecheckTestFJ("Class C, Tnum foo(){return 1+1}, TNum bar(){return foo();} ", ProgramM(C0))(ProgramOK)
//
//  typecheckTestError("Class C, Tnum foo(x TNum){return 1+1}, TNum bar(){return foo();} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))))))
//
//  typecheckTestError("Class C, Tnum foo(x int){return 1+1}, TNum bar(){return foo('a);} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this), Str('a))))))))
//
//  val C1 = ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)),Num(0)))))))
//
//  typecheckTestFJ("Class C, Tnum foo(x int){return 1+1}, TNum bar(){return foo(Num(1));} ", ProgramM(C1))(ProgramOK)
//
//  typecheckTestError("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Num(1));} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Num(1))))))))
//
//  typecheckTestError("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Num(1), string a);} ", ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Num(1), Str('a))))))))
//
//  typecheckTestError("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Var(x), Num(2));} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Var('x), Num(2))))))))
//
//   val C2 = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
//     Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Num(1), Num(1)))),
//       MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)), Num(1), Num(2)))))))
//
//  typecheckTestFJ("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Num(1), Num(2));} ", ProgramM(C2))(ProgramOK)
//
//  val C3 =  ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this))))),
//      MethodDec(Seq(CName('Nat), 'getbar, Seq()), Seq(Invk(Seq('bar), Seq(Var('this)))))))
//  typecheckTestFJ("Class C, Tnum foo(){return 1+1}, TNum bar(){return foo()}, TNum getBar() {return bar} ", ProgramM(C3))(ProgramOK)
//
//  typecheckTestError("Class C, Tnum foo(){return 1+1}, TNum bar(){return foo()}, String getBar() {return bar} ", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Add(Num(1), Num(1)))),
//      MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C)))))),
//      MethodDec(Seq(CName('String), 'getbar, Seq()), Seq(Invk(Seq('bar), Seq(New(CName('C)))))))))//(CName('C))
//
//  typecheckTestError("Class C, TNum add(a Int, b Int){ C.add('a 2)}", ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'add, Seq(('a, CName('Nat)), ('b, CName('Nat)))), Seq(Invk(Seq('add), Seq(New(CName('C)), Str('a), Num(2))))))))
//
//
//  typecheckTestFJ("Class C, Tnum foo(x int, y int){return x+y}} ", ProgramM(ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Var('x), Var('y))))))))(ProgramOK)
//
//  typecheckTestError("Class C, Tnum foo(x int, y int){return z+z}} ", ProgramM(ClassDec(Seq(CName('C), CName('Object), Ctor(ListMap(), ListMap()),Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(('x, CName('Nat)), ('y, CName('Nat)))), Seq(Add(Var('z), Var('z))))))))
//
//  val M1 =  ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Num(1)))))
//
//  val M2 =  ClassDec(Seq(CName('C1), CName('C),Ctor(ListMap(), ListMap()), Seq()),
//    Seq())
//
//  val M3 =  ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('String), 'foo, Seq()), Seq(Str('a)))))
//
//  val M4 =  ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Num(1)))))
//
//  val M5 =  ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
//    Seq())
//
//  val M6 =  ClassDec(Seq(CName('C3), CName('C2),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Num(1)))))
//
//  typecheckTestError("{M1, M2, M3} ok", ProgramM(M1, M2, M3))
//  typecheckTestFJ("{M1, M2, M4} ok", ProgramM(M1, M2, M4))(ProgramOK)
//  typecheckTestFJ("{M1, M2, M5, M6} ok", ProgramM(M1, M2, M5, M6))(ProgramOK)
//  typecheckTestError("{M1, M2, M3, M6} ok", ProgramM(M1, M2, M3, M6))
//
//  val MutualRec = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Invk(Seq('bar), Seq(Var('this))))),
//    MethodDec(Seq(CName('Nat), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this)))))))
//
//  val WMutualRec = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Nat), 'foo, Seq()), Seq(Invk(Seq('bar), Seq(Var('this))))),
//      MethodDec(Seq(CName('String), 'bar, Seq()), Seq(Invk(Seq('foo), Seq(Var('this)))))))
//
//  typecheckTestFJ("{C int foo() {this.bar()}, int bar() {this.foo()}} ok", ProgramM(MutualRec))(ProgramOK)
//  typecheckTestError("{C string foo() {this.bar()}, int bar() {this.foo()}}ok", ProgramM(WMutualRec))
//
//  val mutS1 = ClassDec(Seq(CName('C1), CName('C2),Ctor(ListMap(), ListMap()), Seq()),
//  Seq())
//
//  val mutS2 = ClassDec(Seq(CName('C2), CName('C1),Ctor(ListMap(), ListMap()), Seq()),
//    Seq())
//
//  typecheckTestError("{Mutual Super Types} ok", ProgramM(mutS1, mutS2))
//
//  val ctorFC = ClassDec(Seq(CName('Pair), CName('Object),  Ctor(ListMap(), ListMap('First -> CName('Nat), 'Second -> CName('Nat))),
//    Seq(('First, CName('Nat)), ('Second, CName('Nat)))), Seq())
//
//  val wctorFC = ClassDec(Seq(CName('Pair), CName('Object),  Ctor(ListMap(), ListMap('First -> CName('Nat))),
//    Seq(('First, CName('Nat)), ('Second, CName('Nat)))), Seq())
//
//  typecheckTestFJ("{ctorFC} ok", ProgramM(ctorFC))(ProgramOK)
//  typecheckTestError("{wctorFC} ok", ProgramM(wctorFC))
//
//  val A = ClassDec(Seq(CName('A), CName('Object),Ctor(ListMap(), ListMap('i -> CName('Nat))), Seq(('i, CName('Nat)))),
//    Seq(MethodDec(Seq(CName('Object), 'm, Seq(('b, CName('B)))), Seq(New(CName('B), FieldAcc('i, Var('this)))))))
//
//  val B = ClassDec(Seq(CName('B), CName('A), Ctor(ListMap('i -> CName('Nat)), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Object), 'm, Seq(('b, CName('B)))), Seq(FieldAcc('i, UCast(CName('A),Var('b)))))))
//
//  typecheckTestFJ("{A, B} ok", ProgramM(A, B))(ProgramOK)
//
//  val ASup = ClassDec(Seq(CName('A), CName('Object),Ctor(ListMap(), ListMap()), Seq()), Seq())
//
//  val BSub = ClassDec(Seq(CName('B), CName('A), Ctor(ListMap(), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Object), 'm, Seq()), Seq(FieldAcc('i, Var('this))))))
//
//  typecheckTestError("{ASup, BSub} not ok: unprovided field 'i'", ProgramM(ASup, BSub))
//
//  val AS = ClassDec(Seq(CName('A), CName('Object), Ctor(ListMap('i -> CName('Nat)), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Object), 'm, Seq()), Seq(FieldAcc('i, Var('this))))))
//
//  val BS = ClassDec(Seq(CName('B), CName('Object), Ctor(ListMap(), ListMap('i -> CName('Nat))), Seq(('i, CName('Nat)))),
//    Seq(MethodDec(Seq(CName('Object), 'm, Seq()), Seq(FieldAcc('i, Var('this))))))
//
// // val CS = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()), Seq())
//
//  typecheckTestFJ("{ASup, CSub}", ProgramM(BS))(ProgramOK)


}


class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))

class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubstLateMerge))

class TestBUEarlySolveContinuousSubstCorrectness extends TestCorrectness("BUEarlySolveContinuousSubst", new earlymerge.BUCheckerFactory(SolveContinuousSubstEarlyMerge))
