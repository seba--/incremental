package incremental.pcf.with_references

import incremental.Node._
import incremental.pcf._
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
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
      assert(actual.isLeft, s"Expected resulting type but found type error ${actual.right}")
      assert(expected.isDefinedAt(actual.left.get) && expected(actual.left.get), s"Unexpected type ${actual.left.get}")
    }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest("Ref(17)", Ref(Num(17)))(TRef(TNum))
  typecheckTest("17+Deref(Ref(10+2))", Add(Num(17), Deref(Ref(Add(Num(10), Num(2))))))(TNum)
  typecheckTest("\\x. Deref(x)+Deref(x)", Abs('x, Add(Deref(Var('x)), Deref(Var('x)))))(TFun(TRef(TNum), TNum))
  typecheckTestError("\\x. x+Deref(x)", Abs('x, Add(Var('x), Deref(Var('x)))))
  typecheckTest("\\x. \\y. x=y", Abs('x, Abs('y, Assign(Var('x), Var('y))))){case TFun(TRef(UVar(x)), TFun(UVar(y), TUnit)) => x==y}
  typecheckTest("\\x. \\y. x=y; y", Abs('x, Abs('y, Seq(Assign(Var('x), Var('y)), Var('y))))){case TFun(TRef(UVar(x)), TFun(UVar(y), UVar(z))) => x==y && y==z}
}

class TestDownUpCorrectness extends TestCorrectness("DownUp", DownUpCheckerFactory)
class TestBottomUpCorrectness extends TestCorrectness("BottomUp", BottomUpEagerSubstCheckerFactory)
