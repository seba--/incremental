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
      //println(e.cs.asInstanceOf[JustCollectCS].cons.seq)
    }
  }

  genConstraintTest("17", Lit(Deci("17")))(Seq())
}

class TestBUJustCollect extends TestConstraintCollecting("BUJustCollect", new BUCheckerFactory(JustCollect))