package incremental.java.justCollect

import constraints.CVar
import incremental.Node._
import constraints.javacons._
import incremental.java.syntax._
import incremental.java.syntax.expr._
import incremental.java.JavaCheck._

/**
 * Created by qwert on 19.01.16.
 */
class TestBUCheckJustCollectBooleanOps extends TestBUCheckJustCollect {
  val true_ = Lit(Bool(True()))
  val false_ = Lit(Bool(False()))

  lazy val gtCons = Seq(PrimitiveWidening(TInt(), TLong()),
                        OneOf(TInt(), numTypes),
                        OneOf(TLong(), numTypes))
  genConstraintTest("1>2l", Gt(Lit(Deci("1")), Lit(Deci("2l"))))(gtCons)

  genConstraintTest("true == 0.5f", Eq(true_, Lit(Float("0.5f"))))(Seq(PrimitiveWidening(TBoolean(), TFloat())))

  genConstraintTest("!false", Not(false_))(Seq(Equality(TBoolean(), TBoolean())))
  genConstraintTest("!!!!!false", Not(Not(Not(Not(Not(false_))))))(Seq(Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean())))
  genConstraintTest("true && true", LazyAnd(true_, true_))(Seq(Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean())))
  genConstraintTest("true || false", LazyAnd(true_, false_))(Seq(Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean())))

  lazy val logCons = Seq(Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()), Equality(TBoolean(), TBoolean()))
  genConstraintTest("!True && (False || !True)", LazyAnd(Not(Lit(Bool(True()))), LazyOr(Lit(Bool(False())), Not(Lit(Bool(True()))))))(logCons)

  lazy val condCons = Seq(Equality(TBoolean(),TBoolean()), Equality(TBoolean(),TBoolean()),
                          PrimitiveWidening(TBoolean(),TBoolean()), PrimitiveWideningEq(UVar(CVar('T1)),TBoolean(),TBoolean()),
                          OneOf(TBoolean(), primTypes), OneOf(TBoolean(), primTypes), OneOf(UVar(CVar('T1)), TBoolean() +: numericOpsTypes))

  genConstraintTest("!false ? true : false", Cond(Not(false_), true_, false_))(condCons)
}
