package incremental.java.justCollect

import constraints.CVar
import constraints.javacons._
import incremental.Node._
import incremental.java.JavaCheck._
import incremental.java.syntax._
import incremental.java.syntax.expr._

/**
 * Created by qwert on 02.12.15.
 */
class TestBUCheckJustCollectArithmeticOps extends TestBUCheckJustCollect {
  lazy val plusCons = Seq(OneOf(TInt(), numTypes), OneOf(UVar(CVar('T1)), numericOpsTypes), Equality(TInt(),UVar(CVar('T1))))
  genConstraintTest("+4", Plus(Lit(Deci("4"))))(plusCons)

  lazy val minusCons = Seq(OneOf(TInt(), numTypes), OneOf(UVar(CVar('T2)), numericOpsTypes), Equality(TInt(),UVar(CVar('T2))))
  genConstraintTest("-4", Minus(Lit(Deci("4"))))(minusCons)

  lazy val binPlusCons = Seq(PrimitiveWideningString(TInt(), TInt())
                        , PrimitiveWideningEq(UVar(CVar('T3)), TInt(), TInt())
                        , OneOf(TInt(), TString +: numTypes)
                        , OneOf(TInt(), TString +: numTypes)
                        , OneOf(UVar(CVar('T3)), TString +: numericOpsTypes))
  genConstraintTest("17+4", Plus(Lit(Deci("17")), Lit(Deci("4"))))(binPlusCons)

  lazy val mixedPlusCons = Seq(OneOf(TInt(), numTypes)
                             , OneOf(UVar(CVar('T4)), numericOpsTypes)
                             , Equality(TInt(),UVar(CVar('T4)))
                             , OneOf(TInt(), numTypes)
                             , OneOf(UVar(CVar('T5)), numericOpsTypes)
                             , Equality(TInt(),UVar(CVar('T5)))
                             , PrimitiveWideningString(UVar(CVar('T4)),UVar(CVar('T5)))
                             , PrimitiveWideningEq(UVar(CVar('T6)),UVar(CVar('T4)),UVar(CVar('T5)))
                             , OneOf(UVar(CVar('T4)), TString +: numTypes)
                             , OneOf(UVar(CVar('T5)), TString +: numTypes)
                             , OneOf(UVar(CVar('T6)), TString +: numericOpsTypes))

  genConstraintTest("-17+(+4)", Plus(Minus(Lit(Deci("17"))), Plus(Lit(Deci("4")))))(mixedPlusCons)

  lazy val mulCons = Seq(OneOf(TInt(), numTypes), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T7)), numericOpsTypes)
                       , PrimitiveWidening(TInt(),TInt())
                       , PrimitiveWideningEq(UVar(CVar('T7)),TInt(),TInt()))

  genConstraintTest("17*4", Mul(Lit(Deci("17")), Lit(Deci("4"))))(mulCons)

  lazy val preIncrCons = Seq(Equality(TLong(), UVar(CVar('T8)))
                           , OneOf(TLong(), numTypes)
                           , OneOf(UVar(CVar('T8)), numericOpsTypes))
  genConstraintTest("++1L", PreIncr(Lit(Deci("1L"))))(preIncrCons)
}