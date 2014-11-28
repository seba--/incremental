package incremental.pcf.with_subtyping

import incremental.pcf._
import incremental.Exp._
import incremental.{Type, Util, TypeChecker, TypeCheckerFactory}
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import TypeOps._

/**
 * Created by oliver on 20.11.14.
 */
class TestCorrectness(classdesc: String, checkerFactory: TypeCheckerFactory) extends FunSuite with BeforeAndAfterEach {
  var checker: TypeChecker = _

  override def beforeEach: Unit = {
    checker = checkerFactory.makeChecker
  }
  override def afterEach: Unit = {
    Util.log(f"Preparation time\t${checker.preparationTime}%.3fms")
    Util.log(f"Type-check time\t\t${checker.typecheckTime}%.3fms")
    Util.log(f"Constraint count\t${checker.constraintCount}")
    Util.log(f"Cons. solve time\t${checker.constraintSolveTime}%.3fms")
    Util.log(f"Merge reqs time\t\t${checker.mergeReqsTime}%.3fms")
  }

  import scala.language.implicitConversions
  implicit def eqType(t: Type): PartialFunction[Type,Boolean] = {case t2 => t == t2}

  def typecheckTest(desc: String, e: =>Exp)(expected: PartialFunction[Type,Boolean]): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected resulting type but found type error ${actual.right}")
      assert(expected.isDefinedAt(actual.left.get) && expected(actual.left.get), s"Unexpected type ${actual.left.get}")
    }

  def typecheckTestError(desc: String, e: =>Exp) =
    test (s"$classdesc: Type check error $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest("lambda f: (TNum -> Top) -> TNum. lambda g: Top -> TNum. f g",
    Abs(Seq('f, (TNum -->: Top) -->: TNum), Seq(Abs(Seq('g, Top -->: TNum),  Seq(App(Var('f), Var('g))))))) {
      case ((TNum -->: Top) -->: TNum) -->: (Top -->: TNum) -->: TNum => true
  }
  typecheckTest("lambda f: TNum -> TNum. lambda g: (TNum -> TNum) -> (TNum -> TNum). if0 0 f g",
    Abs(Seq('f, TNum -->: TNum), Seq(Abs(Seq('g, (TNum -->: TNum) -->: (TNum -->: TNum)), Seq(If0(Num(0), Var('f), Var('g))))))) {
      case (TNum -->: TNum) -->: ((TNum -->: TNum) -->: (TNum -->: TNum)) -->: (Bot -->: Top) => true
  }
  typecheckTestError("lambda f: (TNum -> Top) -> TNum. lambda g: TNum. f g",
    Abs(Seq('f, (TNum -->: Top) -->: TNum), Seq(Abs(Seq('g, TNum),  Seq(App(Var('f), Var('g))))))
  )
  typecheckTestError("lambda x: T. x x",
    Abs(Seq('x, TVar('T)), Seq(App(Var('x), Var('x))))
  )
  typecheckTestError("lambda f: TNum -> TNum. f x",
    Abs(Seq('f, TNum -->: TNum), Seq(App(Var('f), Var('x))))
  )
}

class TestBottomUpCorrectness extends TestCorrectness("BottomUp (Subtyping)", BottomUpCheckerFactory)
class TestDownUpCorrectness extends TestCorrectness("DownUp (Subtyping)", DownUpCheckerFactory)
