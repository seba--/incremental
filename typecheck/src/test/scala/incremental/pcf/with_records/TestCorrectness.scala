package incremental.pcf.with_records

import incremental.Exp._
import incremental.pcf._
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by seba on 14/11/14.
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

  def typecheckTest(desc: String, e: =>Exp)(expected: Type) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isLeft, s"Expected resulting type but found type error ${actual.right}")
      assertResult(expected)(actual.left.get)
    }

  def typecheckTestError(desc: String, e: =>Exp) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }

  typecheckTest("Record(l:17)", Record('l, Num(17)))(TRecord(Map('l -> TNum)))
  typecheckTest("Record(l:17, k:\\x. x+x)", Record(Seq('l, 'k), Seq(Num(17), Abs('x, Add(Var('x), Var('x))))))(TRecord(Map('l -> TNum, 'k -> TFun(TNum, TNum))))
  typecheckTest("Record(l:17, k:\\x. x+x).l", Project('l, Record(Seq('l, 'k), Seq(Num(17), Abs('x, Add(Var('x), Var('x)))))))(TNum)
  typecheckTest("Record(l:17, k:\\x. x+x).k", Project('k, Record(Seq('l, 'k), Seq(Num(17), Abs('x, Add(Var('x), Var('x)))))))(TFun(TNum, TNum))
  typecheckTest("\\x. x.m + x.m", Abs('x, Add(Project('m, Var('x)), Project('m, Var('x)))))(TFun(TRecord(Map('m -> TNum)), TNum))
  typecheckTest("\\x. x.m + x.n", Abs('x, Add(Project('m, Var('x)), Project('n, Var('x)))))(TFun(TRecord(Map('m -> TNum, 'n -> TNum)), TNum))
  typecheckTestError("\\x. x.m + (x.m) 0", Abs('x, Add(Project('m, Var('x)), App(Project('m, Var('x)), Num(0)))))
}

class TestDownUpCorrectness extends TestCorrectness("DownUp", DownUpCheckerFactory)
class TestBottomUpCorrectness extends TestCorrectness("BottomUp", BottomUpCheckerFactory)
