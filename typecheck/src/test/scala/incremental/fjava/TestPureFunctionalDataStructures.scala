package incremental.fjava

import benchmark.fjava.Trees
import constraints.fjava._
import constraints.fjava.impl._
import incremental.Node._
import incremental.fjava.latemerge.BUCheckerFactory
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import util.parse.Interpolators._
import util.parse.Parse

class TestPurelyFunctionalDataStructures[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
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

      val sol = SolveContinuousSubstLateMerge.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuousSubstLateMerge.freshConstraintSystem).tryFinalize      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }


  typecheckTest("Purely functional data structures", Parse.prog._1)(ProgramOK)
}


class TestDUSolveEndPurelyFunctionalDataStructures extends TestPurelyFunctionalDataStructures("DUSolveEnd", new DUCheckerFactory(SolveEnd))
class TestBUSolveEndPurelyFunctionalDataStructures extends TestPurelyFunctionalDataStructures("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuousSubstPurelyFunctionalDataStructures extends TestPurelyFunctionalDataStructures("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubstLateMerge))

class TestBUEarlySolveContinuousSubstPurelyFunctionalDataStructures extends TestPurelyFunctionalDataStructures("BUEarlySolveContinuousSubst", new earlymerge.BUCheckerFactory(SolveContinuousSubstEarlyMerge))
