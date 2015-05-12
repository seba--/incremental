package incremental.pcf

import benchmark.ExpGenerator
import constraints.equality
import constraints.equality.ConstraintSystem
import constraints.equality.impl.{SolveContinuousSubstThreshold, SolveContinuousSubst, SolveContinuously, SolveEnd}
import incremental.Node.Node
import incremental.Node._
import incremental._
import incremental.pcf.Exp.Exp
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import ExpGenerator._

import scala.reflect.ClassTag

/**
 * Created by seba on 14/11/14.
 */
class TestScaleNonInc[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

//  override def beforeEach: Unit = {
//    checker = checkerFactory.makeChecker
//  }

  override def afterEach: Unit = checker.localState.printStatistics()

  def typecheckTest(desc: String, e: => Node)(expected: equality.Type): Unit =
    test(s"$classdesc: Type check $desc", SlowTest) {
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
    }

  def scaleTests[T <: Node]
    (heights: Set[Int], kind: (T,T)=>T, leaveMaker: LeaveMaker[T], sharing: Boolean = false, leaveDesc: String = "", wrap : (Int,T) => T = (_:Int,e:T) => e)
    (expected: Int => equality.Type)
    (implicit tag: ClassTag[T]) =
    for (h <- heights)
      scaleTest(h, kind, leaveMaker, sharing, leaveDesc, wrap)(expected)

  def scaleTest[T <: Node]
    (height: Int, kind: (T,T)=>T, leaveMaker: LeaveMaker[T], sharing: Boolean = false, leaveDesc: String = "", wrap : (Int,T) => T = (_:Int,e:T) => e)
    (expected: Int => equality.Type)
    (implicit tag: ClassTag[T]) =
    typecheckTest(
      s"${if(sharing) "shared" else "non-shared"} $kind-tree(h=$height)${if(leaveDesc.isEmpty)"" else " with leaves " + leaveDesc}",
      wrap(height, makeBinTree(height, kind, leaveMaker, sharing)))(expected(height))

  val range = Set(5, 10)
  scaleTests[Exp](range, Add.apply(_,_), constantLeaveMaker(Num(1)), leaveDesc="1 .. 1")(_=>TNum)
  scaleTests[Exp](range, Add.apply(_,_), stateLeaveMaker[Int,Exp](1, i => i + 1, i => Num(i)), leaveDesc="1 .. n")(_=>TNum)
  scaleTests[Exp](range, Add.apply(_,_), constantLeaveMaker(Var('x)), leaveDesc="x .. x", wrap = (h, e) => Abs('x, e))(_=>TFun(TNum, TNum))
  scaleTests[Exp](range, Add.apply(_,_), stateLeaveMaker[Int,Exp](1, i => i + 1, i => Var(Symbol(s"x$i"))), leaveDesc="x1 .. xn", wrap = (h, e) => AbsMany(usedVars(h), e))(h => makeFunType(h, TNum, ()=>TNum, TFun))
}

class TestDUSolveEndNonInc extends TestScaleNonInc("DUSolveEnd", new DUCheckerFactory(SolveEnd))
class TestDUSolveContniuouslyNonInc extends TestScaleNonInc("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndNonInc extends TestScaleNonInc("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveContinuouslyNonInc extends TestScaleNonInc("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
class TestBUSolveContinuousSubstNonInc extends TestScaleNonInc("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuousSubstThresholdNonInc extends TestScaleNonInc("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermNonInc extends TestScaleNonInc("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
