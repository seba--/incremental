package incremental.fjavaMO

import constraints.fjavaMO._
import constraints.fjavaMO.impl._
import incremental.Node._
import incremental.fjavaMO.latemerge.BUCheckerFactory
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

      val sol = SolveContinuousSubstLateMerge.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuousSubstLateMerge.freshConstraintSystem).tryFinalize      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }


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

  //TODO Lira not covering yet the return type, so far only the minsel for param types, but not ret type
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

//  val A = ClassDec(Seq(CName('A), CName('Object),Ctor(ListMap(), ListMap('i -> CName('Nat))), Seq(('i, CName('Nat)))),
//    Seq(MethodDec(Seq(CName('Object), 'm, Seq(('b, CName('B)))), Seq(New(CName('B), FieldAcc('i, Var('this)))))))
//
//  val B = ClassDec(Seq(CName('B), CName('A), Ctor(ListMap('i -> CName('Nat)), ListMap()), Seq()),
//    Seq(MethodDec(Seq(CName('Object), 'm, Seq(('b, CName('B)))), Seq(FieldAcc('i, UCast(CName('A),Var('b)))))))
//
//  typecheckTestFJ("{A, B} ok", ProgramM(A, B))(ProgramOK)

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


  val A = ClassDec(Seq(CName('A), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val B = ClassDec(Seq(CName('B), CName('A), Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val C = ClassDec(Seq(CName('C), CName('B), Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val D = ClassDec(Seq(CName('D), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val E = ClassDec(Seq(CName('E), CName('Object), Ctor(ListMap(), ListMap()), Seq()), Seq(MethodDec(CName('Object), 'm, Seq('x -> CName('A)),Var('this)), MethodDec(CName('Object), 'm, Seq('x -> CName('B)),Var('this)),
    MethodDec(Seq(CName('Object), 'kot, Seq()), Seq(Invk(Seq('m, New(CName('C))), Seq(Var('this)))))))
//
//  val Prove = ClassDec(Seq(CName('Prove), CName('Object), Ctor(ListMap(), ListMap()), Seq()), Seq(MethodDec(Seq(CName('Object), 'kot, Seq()), Seq(Invk(Seq('m, New(CName('C))), Seq(Var('this)))))))

  typecheckTestFJ("{A, B, C, E}", ProgramM(A, B, C, E))(ProgramOK)

  val F = ClassDec(Seq(CName('F), CName('Object), Ctor(ListMap(), ListMap()), Seq()), Seq(MethodDec(CName('Object), 'm, Seq('x -> CName('A)),Var('this)), MethodDec(CName('Object), 'm, Seq('x -> CName('B)),Var('this)),
    MethodDec(CName('Object), 'm, Seq('x -> CName('D)),Var('this)), MethodDec(Seq(CName('Object), 'foo, Seq()), Seq(Invk(Seq('m, New(CName('C))), Seq(Var('this)))))))

  val Asup= ClassDec(Seq(CName('Asub), CName('Object), Ctor(ListMap(), ListMap()), Seq()), Seq(MethodDec(CName('Object), 'm, Seq('x -> CName('A)),Var('this)),
    MethodDec(CName('Object), 'm, Seq('x -> CName('D)),Var('this)), MethodDec(Seq(CName('Object), 'foo, Seq()), Seq(Invk(Seq('m, New(CName('C))), Seq(Var('this)))))))

  val Bsub = ClassDec(Seq(CName('Bsub), CName('Asup), Ctor(ListMap(), ListMap()), Seq()), Seq( MethodDec(CName('Object), 'm, Seq('x -> CName('B)),Var('this))))

  typecheckTestError("not supporting extends yet", ProgramM(A, B, C, Asup, BSub))

  val String = ClassDec(Seq(CName('String), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val StringBuffer = ClassDec(Seq(CName('StringBuffer), CName('Object), Ctor(ListMap(), ListMap()), Seq()),
    Seq())

  val Amb = ClassDec(Seq(CName('Amb), CName('Object), Ctor(ListMap(), ListMap()), Seq()), Seq(MethodDec(CName('Object), 'm, Seq('x -> CName('String)),Var('this)),
    MethodDec(CName('Object), 'm, Seq('x -> CName('StringBuffer)),Var('this)), MethodDec(Seq(CName('Object), 'foo, Seq()), Seq(Invk(Seq('m, New(CName('Object))), Seq(Var('this)))))))

  typecheckTestError("Ambiguous method invocation", ProgramM(String, StringBuffer, Amb))

  //testing merging in presence of method overloading

  val MWr = ClassDec(Seq(CName('MWr), CName('Object), Ctor(ListMap(), ListMap()), Seq()), Seq(MethodDec(CName('Object), 'm, Seq('x -> CName('A)),Var('this)), MethodDec(CName('Object), 'm, Seq('x -> CName('B)),Var('this)),
    MethodDec(CName('Object), 'm, Seq('x -> CName('D)),Var('this)), MethodDec(Seq(CName('Object), 'foo, Seq()), Seq(Invk(Seq('m, New(CName('C))), Seq(Var('this))))),
    MethodDec(Seq(CName('Object), 'bar, Seq()), Seq(Invk(Seq('m, New(CName('D))), Seq(Var('this)))))))


  typecheckTestError("Wrong method merging", ProgramM(A, B, C, D, MWr))

  val MOk = ClassDec(Seq(CName('MOk), CName('Object), Ctor(ListMap(), ListMap()), Seq()), Seq(MethodDec(CName('Object), 'm, Seq('x -> CName('A)),Var('this)), MethodDec(CName('Object), 'm, Seq('x -> CName('B)),Var('this)),
    MethodDec(CName('Object), 'm, Seq('x -> CName('D)),Var('this)), MethodDec(Seq(CName('Object), 'foo, Seq()), Seq(Invk(Seq('m, New(CName('C))), Seq(Var('this))))),
    MethodDec(Seq(CName('Object), 'bar, Seq()), Seq(Invk(Seq('m, New(CName('C))), Seq(Var('this)))))))


  typecheckTestFJ("Ok method merging", ProgramM(A, B, C, D, MOk))(ProgramOK)

}

//class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))

//class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))

class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubstLateMerge))

//class TestBUEarlySolveContinuousSubstCorrectness extends TestCorrectness("BUEarlySolveContinuousSubst", new earlymerge.BUCheckerFactory(SolveContinuousSubstEarlyMerge))
