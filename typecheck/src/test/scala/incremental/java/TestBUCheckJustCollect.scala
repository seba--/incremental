package incremental.java

import constraints.CVar
import constraints.javacons._
import constraints.javacons.impl._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import incremental.Node._
import incremental.java.syntax.expr._

/**
 * Created by qwert on 02.12.15.
 */
class TestBUCheckJustCollect extends FunSuite with BeforeAndAfterEach {
  val checker: TypeChecker[JustCollectCS] = new BUCheckerFactory(JustCollect).makeChecker

  override def afterEach: Unit = checker.localState.printStatistics()

  def genConstraintTest(desc: String, e:  =>Node[Constraint, Result])(expected: Seq[Constraint]): Unit = {
    test (s"BUCheckJustCollect: $desc") {
      val root: Node[Constraint, Result] = e
      val t = checker.typecheck(root)

      assert(t.isLeft, s"unexpected TError")

      val actual = root.cs.asInstanceOf[JustCollectCS].cons
      assert(expected == actual, s"Expected $expected, but got ${actual}")
    }
  }

  genConstraintTest("17", Lit(Deci("17")))(Seq())

  lazy val plusCons = Seq(PrimitiveWideningString(TInt(), TInt())
                        , PrimitiveWideningEq(UVar(CVar('T1)), TInt(), TInt())
                        , OneOf(TInt(), TString +: numTypes)
                        , OneOf(TInt(), TString +: numTypes)
                        , OneOf(UVar(CVar('T1)), TString +: numericOpsTypes))
  genConstraintTest("17+4", Plus(Lit(Deci("17")), Lit(Deci("4"))))(plusCons)
  lazy val logCons = Seq(Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()))
  genConstraintTest("!True && (False || !True)", LazyAnd(Not(Lit(Bool(True()))), LazyOr(Lit(Bool(False())), Not(Lit(Bool(True()))))))(logCons)
}