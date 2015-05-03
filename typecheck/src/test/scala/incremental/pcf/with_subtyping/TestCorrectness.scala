package incremental.pcf.with_subtyping

import constraints.subtype._
import constraints.subtype.impl._
import incremental.Node._
import incremental.Util
import incremental.pcf.{Num, Add, Mul, Abs, App, Fix, If0, Var}
import incremental.pcf.with_subtyping._

import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by oliver on 20.11.14.
 */
class TestCorrectness[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = {
    Util.log(f"Preparation time\t${checker.preparationTime}%.3fms")
    Util.log(f"Type-check time\t\t${checker.typecheckTime}%.3fms")
    Util.log(f"Constraint count\t${checker.constraintCount}")
    Util.log(f"Cons. solve time\t${checker.constraintSolveTime}%.3fms")
    Util.log(f"Merge reqs time\t\t${checker.mergeReqsTime}%.3fms")
    Util.log(f"Finalize time\t\t${checker.finalizeTime}%.3fms")
  }

  import scala.language.implicitConversions
  implicit def eqType(t: Type): PartialFunction[Type,Boolean] = {case t2 => t == t2}

  def typecheckTest(desc: String, e: =>Node)(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected $expected but got $actual")

      val sol = SolveContinuously.state.withValue(checker.csFactory.state.value) {
        Equal(expected, actual.left.get).solve(SolveContinuously.freshConstraintSystem).tryFinalize
      }
      assert(sol.isSolved, s"Expected $expected but got ${actual.left.get}. Match failed with ${sol.unsolved}")
    }
  
  implicit class TypeOps(t: Type) {
    def -->:(t2: Type) = TFun(t2, t)
  }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check error $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest("lambda f: (TInteger -> Top) -> TInteger. lambda g: Top -> TInteger. f g",
    Abs(Seq('f, (TInteger -->: Top) -->: TInteger), Seq(Abs(Seq('g, Top -->: TInteger),  Seq(App(Var('f), Var('g)))))))(
    ((TInteger -->: Top) -->: TInteger) -->: (Top -->: TInteger) -->: TInteger
  )

  typecheckTest("lambda f: TInteger -> TInteger. lambda g: TInteger -> (TInteger -> TInteger). if0 0 f g",
    Abs(Seq('f, TInteger -->: TInteger), Seq(Abs(Seq('g, TInteger -->: (TInteger -->: TInteger)), Seq(If0(Num(0), Var('f), Var('g)))))))(
    (TInteger -->: TInteger) -->: (TInteger -->: (TInteger -->: TInteger)) -->: (TInteger -->: Top)
  )

  typecheckTestError("lambda f: Top. f (If0 0 (lambda x: TInteger. x) (lambda x: TInteger. f))",
    Abs(Seq('f, Top), Seq(App(Var('f), If0(Num(0), Abs(Seq('x, TInteger), Seq(Var('x))), Abs(Seq('x, TInteger), Seq(Var('f)))))))
  )

  typecheckTestError("lambda f: Top. f 0",
    Abs(Seq('f, Top), Seq(App(Var('f), Num(0))))
  )

  typecheckTest("lambda f: TInteger -> TInteger. lambda g: (TInteger -> TInteger) -> (TInteger -> TInteger). if0 0 f g",
    Abs(Seq('f, TInteger -->: TInteger), Seq(Abs(Seq('g, (TInteger -->: TInteger) -->: (TInteger -->: TInteger)), Seq(If0(Num(0), Var('f), Var('g)))))))(
    (TInteger -->: TInteger) -->: ((TInteger -->: TInteger) -->: (TInteger -->: TInteger)) -->: Top
  )

  typecheckTestError("lambda f: (TInteger -> Top) -> TInteger. lambda g: TInteger. f g",
    Abs(Seq('f, (TInteger -->: Top) -->: TInteger), Seq(Abs(Seq('g, TInteger),  Seq(App(Var('f), Var('g))))))
  )

  typecheckTestError("lambda x: T. x x",
    Abs(Seq('x, UVar('T)), Seq(App(Var('x), Var('x))))
  )

//  typecheckTest("(lamba f. (f (lambda x. x)) + (f (lambda y. y)))",
//    Abs('f, Add(App(Var('f), Abs('x, Var('x))), App(Var('f), Abs('y, Var('y)))))
//  ) { case TFun(TFun(TFun(TVar(x), TVar(y)), TInteger), TInteger) if x==y => true }

  typecheckTestError("lambda f: TInteger -> TInteger. f x",
    Abs(Seq('f, TInteger -->: TInteger), Seq(App(Var('f), Var('x))))
  )

  typecheckTest("lambda f: (TInteger -> Top) -> TInteger. f (if0 0 (lambda x: Top. 1) (lambda x: Top. x))",
    Abs(Seq('f, (TInteger -->: Top) -->: TInteger), Seq(App(Var('f), If0(Num(0), Abs(Seq('x, Top), Seq(Num(1))), Abs(Seq('x, Top), Seq(Var('x))))))))(
    ((TInteger -->: Top) -->: TInteger) -->: TInteger
  )

  typecheckTest("fix (lambda x: Top. 1)",
    Fix(Abs(Seq('x, Top), Seq(Num(1)))))(
    Top
  )

  typecheckTest("lambda x: TFloat. lambda sqrt: TFloat -> TFloat. lambda add: TNumeric -> TNumeric -> TNumeric. add x (sqrt x)",
    Abs(Seq('x, TFloat), Seq(Abs(Seq('sqrt, TFloat -->: TFloat), Seq(Abs(Seq('add, TNumeric -->: TNumeric -->: TNumeric), Seq(App(App(Var('add), Var('x)), App(Var('sqrt), Var('x))))))))))(
    TFloat -->: (TFloat -->: TFloat) -->: (TNumeric -->: TNumeric -->: TNumeric) -->: TNumeric
  )

}

class TestDUSolveEndCorrectness extends TestCorrectness("DUSolveEnd", new DUCheckerFactory(SolveEnd))
class TestDUSolveEndCanonicalBoundsCorrectness extends TestCorrectness("DUSolveEndCanonicalBounds", new DUCheckerFactory(SolveEndCanonicalBounds))
class TestDUSolveContinuouslyCorrectness extends TestCorrectness("DUSolveContinuously", new DUCheckerFactory(SolveContinuously))

class TestBUSolveEndCorrectness extends TestCorrectness("BUSolveEnd", new BUCheckerFactory(SolveEnd))
class TestBUSolveEndCanonicalBoundsCorrectness extends TestCorrectness("BUSolveEndCanonicalBounds", new BUCheckerFactory(SolveEndCanonicalBounds))
class TestBUSolveContinuouslyCorrectness extends TestCorrectness("BUSolveContinuously", new BUCheckerFactory(SolveContinuously))
class TestBUSolveContinuousSubstCorrectness extends TestCorrectness("BUSolveContinuousSubst", new BUCheckerFactory(SolveContinuousSubst))
class TestBUSolveContinuousSubstThresholdCorrectness extends TestCorrectness("BUSolveContinuousSubstThreshold", new BUCheckerFactory(SolveContinuousSubstThreshold))
//class TestBottomUpEagerSubstEarlyTermCorrectness extends TestCorrectness("BottomUpEagerSubstEarlyTerm", BottomUpEagerSubstEarlyTermCheckerFactory)
