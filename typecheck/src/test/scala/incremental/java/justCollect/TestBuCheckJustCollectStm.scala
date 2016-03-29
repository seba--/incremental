package incremental.java.justCollect

import constraints.javacons._
import constraints.CVar
import incremental.Node._
import incremental.java.syntax._
import incremental.java.syntax.expr._
import incremental.java.JavaCheck._

/**
 * Created by qwert on 29.03.16.
 */
class TestBuCheckJustCollectStm extends TestBUCheckJustCollect {
  genConstraintTest(";", Empty())(Seq())

  lazy val postIncExprStmCons = Seq(Equality(TInt(), UVar(CVar('T1))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T1)), numericOpsTypes))
  genConstraintTest("1++", ExprStm(PostIncr(Lit(Deci("1")))))(postIncExprStmCons)

  lazy val ifTrueEmptyCons = Seq(Equality(TBoolean(), TBoolean()))
  genConstraintTest("if (true) ;", If(_true, Empty()))(ifTrueEmptyCons)

  lazy val ifElseTrueEmptyCons = Seq(Equality(TBoolean(), TBoolean()))
  genConstraintTest("if (true) ; else ;", If(_true, Empty(), Empty()))(ifElseTrueEmptyCons)

  lazy val ifGtPostIncCons = Seq(Equality(TInt(), UVar(CVar('T2))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T2)), numericOpsTypes), // cond: PostIncr
                                 PrimitiveWidening(TInt(), UVar(CVar('T2))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T2)), numTypes), // cond: Gt
                                 Equality(TInt(), UVar(CVar('T3))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T3)), numericOpsTypes), // body: PostIncr
                                 Equality(TBoolean(),TBoolean())) // cond is boolean
  genConstraintTest("if (1 > 0++) 1++;", If(Gt(Lit(Deci("1")), PostIncr(Lit(Deci("0")))), ExprStm(PostIncr(Lit(Deci("1"))))))(ifGtPostIncCons)

  lazy val ifElseGtPostIncCons = Seq(Equality(TInt(), UVar(CVar('T4))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T4)), numericOpsTypes), // cond: PostIncr
                                     PrimitiveWidening(TInt(), UVar(CVar('T4))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T4)), numTypes), // cond: Gt
                                     Equality(TInt(), UVar(CVar('T5))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T5)), numericOpsTypes), // then: PostIncr
                                     Equality(TInt(), UVar(CVar('T6))), OneOf(TInt(), numTypes), OneOf(UVar(CVar('T6)), numericOpsTypes), // else: PostIncr
                                     Equality(TBoolean(),TBoolean())) // cond is boolean
  genConstraintTest("if (1 > 0++) 1++; else 2++;",
    If(Gt(Lit(Deci("1")), PostIncr(Lit(Deci("0")))),
      ExprStm(PostIncr(Lit(Deci("1")))),
      ExprStm(PostIncr(Lit(Deci("2"))))))(ifElseGtPostIncCons)
}