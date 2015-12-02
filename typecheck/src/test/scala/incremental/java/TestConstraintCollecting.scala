package incremental.java

import constraints.javacons._
import constraints.javacons.impl._
import incremental.java.JavaCheck.Result
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import incremental.Node._
import incremental.java.syntax.expr._

/**
 * Created by qwert on 02.12.15.
 */
class TestConstraintCollecting[CS <: ConstraintSystem[CS]](classdesc: String, checkerFactory: TypeCheckerFactory[CS]) extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[CS] = checkerFactory.makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def genConstraintTest(desc: String, e:  =>Node[Constraint, Result])(expected: Seq[Constraint]): Unit = {
    test (s"$classdesc: Type check $desc") {
      val actual = checker.typecheck(e)

      assert(actual.isLeft, s"Expected $expected but got $actual")

      println(actual.left.get)

      //val cons = e.withCS[JustCollectCS].cs.cons
      //println(e.cs.asInstanceOf[JustCollectCS].cons)
      //println(e.withCS[JustCollectCS].cs.getConstraints)
    }
  }

  genConstraintTest("17", Lit(Deci("17")))(Seq())
  genConstraintTest("17+4", Plus(Lit(Deci("17")), Lit(Deci("4"))))(Seq())
  genConstraintTest("!True && (False || !True)", LazyAnd(Not(Lit(Bool(True()))), LazyOr(Lit(Bool(False())), Not(Lit(Bool(True()))))))(Seq())
}

class TestBUJustCollect extends TestConstraintCollecting("BUJustCollect", new BUCheckerFactory(JustCollect))