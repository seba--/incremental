package incremental.fjava

import constraints.fjava._
import constraints.fjava.impl._
import incremental.Node._
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by lirakuci on 3/29/15.
 */
class TestNat[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: BUCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: BUChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def typecheckTest(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val ev = e
      val actual = checker.typecheck(ev)

      val typ = ev.withType[checker.Result].typ._1
      val req = ev.withType[checker.Result].typ._2
      val creq = ev.withType[checker.Result].typ._3
      val cons = ev.withType[checker.Result].typ._4
      assert(actual.isLeft, s"Expected $expected but got Type = $typ, Reqs = $req, CReqs = $creq, Constraint = $cons")

      val sol = SolveContinuousSubst.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuousSubst.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }


  val Nat = ClassDec(
    Seq(CName('Nat), CName('Object),
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
        Var('this)) // dummy body, will be overwritten by subclasses
    )
  )
  typecheckTest("Nat ok", Nat)(CName('Nat))

  val Zero = ClassDec(
    Seq(CName('Zero), CName('Nat),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        New(CName('Succ), Var('this))),
      MethodDec(
        CName('Nat), 'pred, Seq(),
        Var('this)), // pred of Zero is Zero
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        Var('other))
    )
  )
  // without Zero knowing Succ, the class fails to check
  typecheckTestError("Zero ok", Zero)

  val Succ = ClassDec(
    Seq(CName('Succ), CName('Nat),
      Seq('x -> CName('Nat))), // x is the predecessor of this nat
    Seq(
      MethodDec(
        CName('Nat), 'succ, Seq(),
        New(CName('Succ), Var('this))),
      MethodDec(
        CName('Nat), 'pred, Seq(),
        Fields('x, Var('this))), // pred of Zero is Zero
      MethodDec(
        CName('Nat), 'plus, Seq('other -> CName('Nat)),
        New(CName('Succ), Invk('plus, Fields('x, Var('this)), Var('other)))) // plus(Succ(x), other) = Succ(plus(x, other))
    )
  )
  // Succ refers to Nat and should fail to check
  typecheckTestError("Succ ok", Succ)

  // Taking all classes into consideration, checking should succeed
  typecheckTest("{Nat, Zero, Succ} ok", ProgramM(Nat, Zero, Succ))(CName('Object))


}

class TestBUSolveEndNat extends TestNat("BUSolveEnd", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))

