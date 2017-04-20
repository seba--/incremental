package incremental.fjava

import constraints.fjava._
import constraints.fjava.impl._
import incremental.Node._
import incremental.fjava.latemerge.BUCheckerFactory
import org.scalatest.{BeforeAndAfterEach, FunSuite}

import scala.collection.immutable.ListMap

/**
 * Created by lirakuci on 3/29/15.
 */
class TestLocalVars[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  val nats = new Nats
  val strings = new Strings

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


   val C2 = ClassDec(Seq(CName('C), CName('Object),Ctor(ListMap(), ListMap()), Seq()),
     Seq(MethodDec(Seq(CName('Nat), 'foo, Seq(), Seq('i, CName('Nat), ('j, CName('Nat)))), Seq(Invk(Seq('foo), Seq(Var('this))))),
       MethodDec(Seq(CName('Nat), 'bar, Seq(),Seq()), Seq(Invk(Seq('foo), Seq(New(CName('C))))))))

  typecheckTestFJ("Class C, Tnum foo(x int, y int){return 1+1}, TNum bar(){return foo(Num(1), Num(2));} ", ProgramM(C2))(ProgramOK)

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


}

//class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))

class TestBUSolveEndLocalVars extends TestLocalVars("BUSolveEnd", new BUCheckerFactory(SolveEnd))

//class TestBUSolveContinuousSubstLocalVars extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))

//class TestBUEarlySolveContinuousSubstCorrectness extends TestCorrectness("BUEarlySolveContinuousSubst", new earlymerge.BUCheckerFactory(SolveContinuousSubst))
