package incremental.pcf.with_subtyping

import incremental.pcf._
import incremental.Node._
import incremental.{Util, TypeChecker, TypeCheckerFactory}
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import TypeOps._

/**
 * Created by oliver on 20.11.14.
 */
class TestCorrectness(classdesc: String, checkerFactory: TypeCheckerFactory[Type]) extends FunSuite with BeforeAndAfterEach {
  var checker: TypeChecker[Type] = _

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

  def typecheckTest(desc: String, e: =>Node)(expected: PartialFunction[Type,Boolean]): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected resulting type but found type error ${actual}")
      assert(expected.isDefinedAt(actual.left.get) && expected(actual.left.get), s"Unexpected type ${actual.left.get}")
    }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check error $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }
  typecheckTest("lambda f: (TNum -> Top) -> TNum. lambda g: Top -> TNum. f g",
    Abs(Seq('f, (TNum -->: Top) -->: TNum), Seq(Abs(Seq('g, Top -->: TNum),  Seq(App(Var('f), Var('g))))))) {
      case ((TNum -->: Top) -->: TNum) -->: (Top -->: TNum) -->: TNum => true
  }
  typecheckTest("lambda f: TNum -> TNum. lambda g: TNum -> (TNum -> TNum). if0 0 f g",
    Abs(Seq('f, TNum -->: TNum), Seq(Abs(Seq('g, TNum -->: (TNum -->: TNum)), Seq(If0(Num(0), Var('f), Var('g))))))) {
    case (TNum -->: TNum) -->: (TNum -->: (TNum -->: TNum)) -->: (TNum -->: Top) => true
  }
  typecheckTestError("lambda f: Top. f (If0 0 (lambda x: TNum. x) (lambda x: TNum. f))",
    Abs(Seq('f, Top), Seq(App(Var('f), If0(Num(0), Abs(Seq('x, TNum), Seq(Var('x))), Abs(Seq('x, TNum), Seq(Var('f)))))))
  )
  typecheckTestError("lambda f: Top. f 0",
    Abs(Seq('f, Top), Seq(App(Var('f), Num(0))))
  )
  typecheckTest("lambda f: TNum -> TNum. lambda g: (TNum -> TNum) -> (TNum -> TNum). if0 0 f g",
    Abs(Seq('f, TNum -->: TNum), Seq(Abs(Seq('g, (TNum -->: TNum) -->: (TNum -->: TNum)), Seq(If0(Num(0), Var('f), Var('g))))))) {
    case (TNum -->: TNum) -->: ((TNum -->: TNum) -->: (TNum -->: TNum)) -->: Top => true
  }
  typecheckTestError("lambda f: (TNum -> Top) -> TNum. lambda g: TNum. f g",
    Abs(Seq('f, (TNum -->: Top) -->: TNum), Seq(Abs(Seq('g, TNum),  Seq(App(Var('f), Var('g))))))
  )
  typecheckTestError("lambda x: T. x x",
    Abs(Seq('x, UVar('T)), Seq(App(Var('x), Var('x))))
  )
//  typecheckTest("(lamba f. (f (lambda x. x)) + (f (lambda y. y)))",
//    Abs('f, Add(App(Var('f), Abs('x, Var('x))), App(Var('f), Abs('y, Var('y)))))
//  ) { case TFun(TFun(TFun(TVar(x), TVar(y)), TNum), TNum) if x==y => true }
  typecheckTestError("lambda f: TNum -> TNum. f x",
    Abs(Seq('f, TNum -->: TNum), Seq(App(Var('f), Var('x))))
  )
  typecheckTest("lambda f: (TNum -> Top) -> TNum. f (if0 0 (lambda x: Top. 1) (lambda x: Top. x))",
    Abs(Seq('f, (TNum -->: Top) -->: TNum), Seq(App(Var('f), If0(Num(0), Abs(Seq('x, Top), Seq(Num(1))), Abs(Seq('x, Top), Seq(Var('x)))))))
  ) {
    case ((TNum -->: Top) -->: TNum) -->: TNum => true
  }
  typecheckTest("fix (lambda x: Top. 1)",
    Fix(Abs(Seq('x, Top), Seq(Num(1))))) {
    case Top => true
  }
  typecheckTest("lambda x: TFloat. lambda sqrt: TFloat -> TFloat. lambda add: TNumeric -> TNumeric -> TNumeric. add x (sqrt x)",
    Abs(Seq('x, TFloat), Seq(Abs(Seq('sqrt, TFloat -->: TFloat), Seq(Abs(Seq('add, TNumeric -->: TNumeric -->: TNumeric), Seq(App(App(Var('add), Var('x)), App(Var('sqrt), Var('x))))))
  )))) {
    case TFloat -->: (TFloat -->: TFloat) -->: (TNumeric -->: TNumeric -->: TNumeric) -->: TNumeric => true
  }
}

class TestBottomUpCorrectness extends TestCorrectness("BottomUp (Subtyping)", BottomUpCheckerFactory)
class TestDownUpCorrectness extends TestCorrectness("DownUp (Subtyping)", DownUpCheckerFactory)
