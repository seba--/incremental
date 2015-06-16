package incremental.Java

import incremental.Node._
import incremental.Java.syntax._
//import incremental.SyntaxChecking.SyntaxChecker.SyntaxError
import org.scalatest.FunSuite

/**
 * Created by qwert on 19.05.15.
 */
class TestSyntax extends FunSuite{
  def syntaxTest(desc: String, e: => Node): Unit = {
    test (s"Syntax: $desc") {
      val v = e
    }
  }

  def errornousSyntaxTest(desc: String, e: => Node): Unit = {
    test (s"Errornous syntax: $desc") {
      intercept[IllegalArgumentException] {
        val v = e
      }

      /*try {
        val v = e
        fail()
      } catch {
        //case err: IllegalArgumentException => err.isInstanceOf[SyntaxError]
        //case err: SyntaxError => ;
        case err: IllegalArgumentException => ;
      }*/
    }
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
  errornousSyntaxTest("int ? 1 : 2", Cond(TInt(), Lit(Deci("1")), Lit(Deci("2"))))
  errornousSyntaxTest("true ? 1 : ", Cond(Lit(True()), Lit(Deci("1"))))

  // Casts
  syntaxTest("(float) (2+2)", CastPrim(TFloat(), Plus(Lit(Deci("2")), Lit(Deci("2")))))
  syntaxTest("(java.lang.String) 2", CastRef(ClassType(TypeNameExt(PackageOrTypeNameExt(PackageOrTypeNameT("java"), "lang"), "String"), None), Lit(Deci("2"))))
  syntaxTest("\"foo\" instanceOf String", InstanceOf(ClassType(TypeNameT("String"), None), Lit(StringL("foo"))))
  errornousSyntaxTest("\"foo\" instanceOf int", InstanceOf(TInt(), Lit(StringL("foo"))))

  // Assignments
  syntaxTest("var = 5", Assign(ExprName("var"), Lit(Deci("5"))))
  syntaxTest("var += 5", AssignPlus(ExprName("var"), Lit(Deci("5"))))
  syntaxTest("this.var = 5", Assign(Field("var", This()), Lit(Deci("5"))))
  syntaxTest("super.var = 5", Assign(SuperField("var"), Lit(Deci("5"))))
  syntaxTest("arr[2] = 5", Assign(ArrayAccess(ExprName("arr"), Lit(Deci("2"))), Lit(Deci("5"))))
  errornousSyntaxTest("foo = 2", Assign(Lit(StringL("foo")), Lit(Deci("2"))))
  errornousSyntaxTest("foo() = 2", Assign(Invoke(Method(MethodNameT("foo"))), Lit(Deci("2"))))

  // Arrays
  syntaxTest("new int[5]", NewArray(TInt(), DimExpr(Lit(Deci("5")))))
  syntaxTest("new char[1][2][]", NewArray(TChar(), DimExpr(Lit(Deci("1"))), DimExpr(Lit(Deci("2"))), Dim()))
  syntaxTest("new String[1+2]", NewArray(TypeNameT("String"), DimExpr(Plus(Lit(Deci("1")), Lit(Deci("2"))))))
  errornousSyntaxTest("new float[][1]", NewArray(TFloat(), Dim(), DimExpr(Lit(Deci("1")))))
  errornousSyntaxTest("new double[1][][2]", NewArray(TDouble(), DimExpr(Lit(Deci("1"))), Dim(), DimExpr(Lit(Deci("2")))))
  errornousSyntaxTest("new boolean ++5", NewArray(TBoolean(), PreIncr(Lit(Deci("5")))))

  // Method Invokation
  syntaxTest("foo()", Invoke(Method(MethodNameT("foo"))))
  syntaxTest("foo(1)", Invoke(Method(MethodNameT("foo")), Lit(Deci("1"))))
  syntaxTest("foo(1, 2)", Invoke(Method(MethodNameT("foo")), Lit(Deci("1")), Lit(Deci("2"))))
  syntaxTest("foo(1, 2, 3)", Invoke(Method(MethodNameT("foo")), Lit(Deci("1")), Lit(Deci("2")), Lit(Deci("3"))))
  syntaxTest("super.foo()", Invoke(SuperMethod("foo")))
  syntaxTest("super.foo(1, 2, 3)", Invoke(SuperMethod("foo"), Lit(Deci("1")), Lit(Deci("2")), Lit(Deci("3"))))
  errornousSyntaxTest("1+2()", Invoke(Plus(Lit(Deci("1")), Lit(Deci("2")))))

  // Mixed tests for Expr
  errornousSyntaxTest("nesting deci literal 2", Lit(Lit(Deci("2"))))
  errornousSyntaxTest("scala integers in Plus", Plus(1, 2))
  errornousSyntaxTest("scala types in CastPrim", CastPrim(Int, Lit(Deci("2"))))
  errornousSyntaxTest("Stmt in Expr", PreIncr(Empty()))
  errornousSyntaxTest("Too many arguments in shift", LeftShift(Lit(Deci("1")), Lit(Deci("1")), Lit(Deci("1"))))

  // Field Declaration
  syntaxTest("vardec: foo", VarDec(ID("foo")))
  syntaxTest("vardec: foo = 5", VarDec(ID("foo"), Lit(Deci("5"))))
  syntaxTest("vardec: foo = 1+2", VarDec(ID("foo"), Plus(Lit(Deci("1")), Lit(Deci("2")))))
  //lazy val varDecFoo = VarDec(ID("foo"))
  //lazy val varDecFooInit = VarDec(ID("foo"), Lit(Deci("5")))
  syntaxTest("field: int foo;", FieldDec(TInt(), VarDec(ID("foo"))))
  syntaxTest("field: int foo, bar;", FieldDec(TInt(), VarDec(ID("foo")), VarDec(ID("bar"))))
  syntaxTest("field: public double foo = 5;", FieldDec(Public(), TDouble(), VarDec(ID("foo"), Lit(Deci("5")))))
  syntaxTest("field: public static final double foo = 5;", FieldDec(Seq(Public(), Static(), Final(), TDouble()), Seq(VarDec(ID("foo"), Lit(Deci("5"))))))

  // Annotation Types
  lazy val annoDecHead = AnnoDecHead(Private(), "foo")
  lazy val annoMethodDec = AnnoMethodDec(TInt(), "bar")
  syntaxTest("anno dec", AnnoDec(annoDecHead, annoMethodDec))

  // Abstract Method Dec
  syntaxTest("void foo();", AbstractMethodDec(Void(), "foo"))
  syntaxTest("public void foo();", AbstractMethodDec(Seq(Public(), Void(), "foo"), Seq()))
  syntaxTest("public void foo(int a);", AbstractMethodDec(Seq(Public(), Void(), "foo"), Seq(Param(TInt(), ID("a")))))
  syntaxTest("public void foo(int a, int b);", AbstractMethodDec(Seq(Public(), Void(), "foo"), Seq(Param(TInt(), ID("a")), Param(TInt(), ID("b")))))
  syntaxTest("public abstract int foobar() throws Exception;", AbstractMethodDec(Seq(Public(), Abstract(), TInt(), "foobar", ClassType(TypeNameT("Exception"), None)), Seq()))
  syntaxTest("public abstract int foobar(double a, char c) throws Exception;", AbstractMethodDec(Seq(Public(), Abstract(), TInt(), "foobar", ClassType(TypeNameT("Exception"), None)), Seq(Param(TDouble(), ID("a")), Param(TChar(), ID("c")))))
  errornousSyntaxTest("private abstract void foo(int a);", AbstractMethodDec(Seq(Private(), Abstract(), Void(), "foo"), Seq(Param(TInt(), ID("a")))))
}