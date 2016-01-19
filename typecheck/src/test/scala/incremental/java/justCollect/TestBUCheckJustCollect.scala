package incremental.java.justCollect

import constraints.javacons._
import constraints.javacons.impl._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.{BUCheckerFactory, TypeChecker}
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by qwert on 19.01.16.
 */
class TestBUCheckJustCollect extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[JustCollectCS] = new BUCheckerFactory(JustCollect).makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def genConstraintTest(desc: String, e:  =>Node[Constraint, Result])(expected: Seq[Constraint]): Unit = {
    test (s"BUCheckJustCollect: $desc") {
      val root: Node[Constraint, Result] = e
      val t = checker.typecheck(root)

      val actual = root.cs.asInstanceOf[JustCollectCS].cons
      assert(expected == actual, s"Expected $expected, but got ${actual}")
    }
  }
}