package incremental.FJava

import incremental.Node._
import incremental.systemf.TVar
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by lirakuci on 3/29/15.
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

  def typecheckTest(desc: String, e: =>Node)(expected: Type): Unit =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assertResult(Left(expected))(actual)
    }

  def typecheckTestError(desc: String, e: =>Node) =
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)
      assert(actual.isRight, s"Expected type error but got $actual")
    }


  typecheckTestError("x", Var('x))
  typecheckTestError("e0.f : U ", Field('f,Var('e0)))
  typecheckTestError("new C(x):C", New(CName('c),Var('x)))
  typecheckTestError("e0.m(e) : U", Invk('m, Var('e0), Var('e)))
  typecheckTestError("(C) e0 :C", UCast(CName('c),Var('e)))
  typecheckTestError("y", Var('x))
  typecheckTestError(" (new Pair(first)).first : U", Field('first,New(CName('Pair),Var('first))))
  typecheckTestError("new Pair(first) : Pair", New(CName('Pair),Var('first)))
  typecheckTestError("new Pair(snd): Pair", New(CName('Pair),Var('object)))
  typecheckTestError("Pair.setfst(first) : U ", Invk('setfst,New(CName('Pair),Var('first)), Var('first)))
  typecheckTestError("(Object)first : Object", UCast(CName('Object),Var('first)))
  typecheckTestError("(Pair) first : Pair", New(CName('Pair),Var('first)))
  typecheckTestError("new Object()", New(CName('Object)))
  //  typecheckTestError("(C) e0 : C", DCast(CName('c),'e))
  //  typecheckTestError("(C) e0 : C", SCast(CName('c),'e))
  typecheckTestError("Int getX(x) {return Int} in Number", Method( CName('Int), 'getX, CName('Int), CName('Number), Var('x)))
}

class TestBottomUpCorrectness extends TestCorrectness("BottomUp FJava", BottomUpCheckerFactory)

