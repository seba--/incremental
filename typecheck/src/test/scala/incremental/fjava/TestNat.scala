package incremental.fjava

import constraints.fjava._
import constraints.fjava.impl._
import incremental.Node._
import incremental.fjava.latemerge.{BUChecker, BUCheckerFactory}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

import scala.collection.immutable.ListMap

/**
 * Created by lirakuci on 3/29/15.
 */

class Nats {
  val Nat = ClassDec(
    Seq(CName('Nat), CName('Object),  Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        Var('this)), // dummy body, will be overwritten by subclasses
      MethodDec(
        CName('Nat), 'pred, Seq(),
        Var('this)), // dummy body, will be overwritten by subclasses
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        Var('this )) // dummy body, will be overwritten by subclasses
    )
  )

  val Zero = ClassDec(
    Seq(CName('Zero), CName('Nat),  Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        Var('this)),
      MethodDec(
        CName('Nat), 'pred, Seq(),
        Var('this)), // pred of Zero is Zero
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        Var('other))
    )
  )

  val Succ = ClassDec(
    Seq(CName('Succ), CName('Nat),  Ctor(ListMap(), ListMap('x -> CName('Nat))),
      Seq(('x -> CName('Nat)))), // x is the predecessor of this nat
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        New(CName('Succ), New(CName('Nat)))), // --- New(CName('Succ), Var('this))),
      MethodDec(
        CName('Nat), 'pred, Seq(),
        FieldAcc('x, Var('this))), // pred of Zero is Zero -- FieldAcc('x, New(CName('Succ), Var('this))))
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        New(CName('Succ), Invk('plus, FieldAcc('x, Var('this)), Var('other)))) // plus(Succ(x), other) = Succ(plus(x, other))
    )
  )

  val all = Seq(Nat, Zero, Succ)

  def Num(n: Int): Node = if (n == 0) New(CName('Zero)) else New(CName('Succ), Num(n-1))

  def Add(e1: Node, e2: Node) = Invk('plus, e1, e2)
}

class Strings {
  val string = ClassDec(
    Seq(CName('String), CName('Object), Ctor(ListMap(), ListMap('o -> CName('Object))), Seq('o -> CName('Object))),
    Seq()
  )

  def Str(s: Any) = New(CName('String), New(CName('Object)))
}

class TestNat[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  val nats = new Nats

  import nats._

  def typecheckTest(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val ev = e
      val actual = checker.typecheck(ev)

//      val typ = ev.withType[checker.Result].typ._1
//      val req = ev.withType[checker.Result].typ._2
//      val creq = ev.withType[checker.Result].typ._3
//      val cons = ev.withType[checker.Result].typ._4
      assert(actual.isLeft, actual.right)

      val sol = SolveContinuousSubstLateMerge.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuousSubstLateMerge.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }


  typecheckTest("Nat ok", ProgramM(Nat))(ProgramOK)

  // without Zero knowing Succ, the class fails to check
  typecheckTestError("Zero ok", ProgramM(Zero))

  // Succ refers to Nat and should fail to check
  typecheckTestError("Succ ok", ProgramM(Succ))

  // Taking all classes into consideration, checking should succeed
  typecheckTest("{Nat, Zero, Succ} ok", ProgramM(Nat, Zero, Succ))(ProgramOK)

}

class TestJavacNat extends TestNat("JAVAC", new JavacCheckerFactory(SolveEnd))

class TestDUSolveEndNat extends TestNat("DUSolveEnd", new DUCheckerFactory(SolveEnd))
class TestBUSolveEndNat extends TestNat("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuousSubstNat extends TestNat("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubstLateMerge))

class TestBUEarlySolveContinuousSubstNat extends TestNat("BUEarlySolveContinuousSubst", new earlymerge.BUCheckerFactory(SolveContinuousSubstEarlyMerge))
