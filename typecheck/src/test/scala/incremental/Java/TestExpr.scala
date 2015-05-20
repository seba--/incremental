package incremental.Java

import incremental.Node._
import incremental.{Type, TypeChecker, TypeCheckerFactory, Util}
import incremental.Java.syntax._
import org.scalatest.{BeforeAndAfterEach, FunSuite}

/**
 * Created by qwert on 19.05.15.
 */
class Test {
  def syntaxTest(desc: String, e: => Node): Unit = {
    // nothing to do?
  }

  // Literals
  syntaxTest("deci 17", Lit(Deci("17")))
  syntaxTest("hexa 17", Lit(Hexa("0x17")))
  syntaxTest("string lit", Lit(StringL("foo")))
  syntaxTest("null", Lit(Null()))
  syntaxTest("void", Lit(VoidClass()))
  syntaxTest("this", This())
  syntaxTest("QThis", QThis(TypeNameExt(PackageOrTypeNameT("pkg"), "obj")))

  // Comparison Operators
  syntaxTest("1 > 2", Gt(Lit(Deci("1")), Lit(Deci("2"))))
  syntaxTest("2 > 1", Gt(Lit(Deci("2")), Lit(Deci("1"))))
  syntaxTest("10 => 5", GtEq(Lit(Deci("10")), Lit(Deci("5"))))
  syntaxTest("1 < 1", Lt(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("1 <= 1", LtEq(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("0 == 1", Eq(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("foo == 2", Eq(Lit(StringL("foo")), Lit(Deci("2"))))
  syntaxTest("10.01f != bar", NotEq(Lit(Float("10.01f")), Lit(StringL("bar"))))

  // Arithmetic Operators
  syntaxTest("+1", Plus(Lit(Deci("1"))))
  syntaxTest("1+1", Plus(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("-1", Minus(Lit(Deci("1"))))
  syntaxTest("1-1", Minus(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("1*1", Mul(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("1/1", Div(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("1%1", Remain(Lit(Deci("1")), Lit(Deci("1"))))
  syntaxTest("((1+1)-(1/1))*(1%1)", Mul(Minus(Plus(Lit(Deci("1")), Lit(Deci("1"))), Div(Lit(Deci("1")), Lit(Deci("1")))), Remain(Lit(Deci("1")), Lit(Deci("1")))))
  syntaxTest("1++", PostIncr(Lit(Deci("1"))))
  syntaxTest("++1", PreIncr(Lit(Deci("1"))))
  syntaxTest("++(1++)", PreIncr(PostIncr(Lit(Deci("1")))))
  syntaxTest("--2", PreDecr(Lit(Deci("2"))))
  syntaxTest("2--", PostDecr(Lit(Deci("2"))))
  syntaxTest("--(1++)", PreDecr(PostIncr(Lit(Deci("1")))))
  syntaxTest("--((++2)+(2++))", PreDecr(Plus(PreIncr(Lit(Deci("2"))), PostIncr(Lit(Deci("2"))))))
  syntaxTest("1 << 2", LeftShift(Lit(Deci("1")), Lit(Deci("2"))))
  syntaxTest("1 >> 2", RightShift(Lit(Deci("1")), Lit(Deci("2"))))
  syntaxTest("1 >>> 2", URightShift(Lit(Deci("1")), Lit(Deci("2"))))
  syntaxTest("(1 << 2)+(--(2/2))", Plus(LeftShift(Lit(Deci("1")), Lit(Deci("2"))), PreDecr(Div(Lit(Deci("2")), Lit(Deci("2"))))))

  // Bitwise Operators
  syntaxTest("~(2+2)", Complement(Plus(Lit(Deci("2")), Lit(Deci("2")))))
  syntaxTest("2&2", And(Lit(Deci("2")), Lit(Deci("2"))))
  syntaxTest("(2&2)|~(2+2)", Or(And(Lit(Deci("2")), Lit(Deci("2"))), Complement(Plus(Lit(Deci("2")), Lit(Deci("2"))))))

  // Conditional Operator
  syntaxTest("1>2 ? ~(10) : 2++", Cond(Gt(Lit(Deci("1")), Lit(Deci("2"))), Complement(Lit(Deci("10"))), PostIncr(Lit(Deci("2")))))

  // Casts
  syntaxTest("(float) (2+2)", CastPrim(TFloat(), Plus(Lit(Deci("2")), Lit(Deci("2")))))

  // Assignments
  syntaxTest("var = 5", Assign(ExprNameT("var"), Lit(Deci("5"))))
  syntaxTest("var += 5", AssignPlus(ExprNameT("var"), Lit(Deci("5"))))
}

