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
class TestBoolean[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def typecheckTest(desc: String, e: => Node)(expected: Type): Unit =
    test(s"$classdesc: Type check $desc") {
      val ev = e
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


  val Bool = ClassDec(
    Seq(CName('Bool), CName('Object), Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Object), 'not, Seq(),
        New(CName('Bool))), // dummy body, will be overwritten by subclasses
      MethodDec(
        CName('Object), 'ifTrue, Seq('vthen -> CName('Object), 'velse -> CName('Object)),
        New(CName('Object))) // dummy body, will be overwritten by subclasses
    )
  )
  typecheckTest("Bool ok", ProgramM(Bool))(ProgramOK)

  val True = ClassDec(
    Seq(CName('True), CName('Bool), Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Object), 'not, Seq(),
        New(CName('False))),
      MethodDec(
        CName('Object), 'ifTrue, Seq('vthen -> CName('Object), 'velse -> CName('Object)),
        Var('vthen))
    )
  )
  val False = ClassDec(
    Seq(CName('False), CName('Bool),Ctor(ListMap(), ListMap()),
      Seq()), // no fields
    Seq(
      MethodDec(
        CName('Object), 'not, Seq(),
        New(CName('True))),
      MethodDec(
        CName('Object), 'ifTrue, Seq('vthen -> CName('Object), 'velse -> CName('Object)), //was bool, found the erroe by hte cons solver :)
        Var('velse))
    )
  )

  // without True knowing False (and vice versa), the class fails to check
  typecheckTestError("True ok", True)
  typecheckTestError("False ok", False)

  // Taking all classes into consideration, checking should succeed
  typecheckTest("{Boolean, True, False} ok", ProgramM(Bool, True, False))(ProgramOK)
}

class TestBUSolveEndBoolean extends TestBoolean("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuousSubstBoolean extends TestBoolean("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))

class TestBUEarlySolveEndBoolean extends TestBoolean("BUEarlySolveEnd", new earlymerge.BUCheckerFactory(SolveEnd))
class TestBUEarlySolveContinuousSubstBoolean extends TestBoolean("BUEarlySolveContinuousSubst", new earlymerge.BUCheckerFactory(SolveContinuousSubst))
