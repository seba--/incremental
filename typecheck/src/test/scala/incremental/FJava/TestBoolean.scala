package incremental.FJava

import constraints.CVar
import constraints.equality._
import constraints.equality.impl._
import incremental.Node._
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by lirakuci on 3/29/15.
 */
class TestBoolean[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: BUCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
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

      val sol = SolveContinuously.state.withValue(checker.csFactory.state.value) {
        expected.unify(actual.left.get, SolveContinuously.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }

  def typecheckTestError(desc: String, e: => Node) =
    test(s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }


  val Bool = ClassDec(
    Seq(CName('Bool), CName('Object),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Bool), 'not, Seq(),
        New(CName('Bool))) // dummy body, will be overwritten by subclasses
    )
  )
  typecheckTest("Bool ok", Bool)(CName('Bool))

  val True = ClassDec(
    Seq(CName('True), CName('Bool),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Bool), 'not, Seq(),
        New(CName('False)))
    )
  )
  val False = ClassDec(
    Seq(CName('False), CName('Bool),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Bool), 'not, Seq(),
        New(CName('True)))
    )
  )

  // without True knowing False (and vice versa), the class fails to check
  typecheckTestError("True ok", True)
  typecheckTestError("False ok", False)

  // Taking all classes into consideration, checking should succeed
  typecheckTest("{Boolean, True, False} ok", ProgramM(Bool, True, False))(ProgramOK)


}

class TestBUSolveEndBoolean extends TestBoolean("BUSolveEnd", new BUCheckerFactory(SolveEnd))
//class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
//class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
//class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))

