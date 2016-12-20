package experiment

import scala.language.implicitConversions

import scala.virtualization.lms.common._

trait StagedEval extends ScalaOpsPkgExp with MapOpsExp {
  type Env = Map[String, Val]
  type Dom = Rep[Env => Val]

  trait Val
  case class NumVal(n: Int) extends Val
  case class FunVal(x: String, e: Dom, env: Rep[Env]) extends Val {
    override def toString = s"FunVal($x, $e, _env_)"
  }
  case class RecVal(var x: Val) extends Val

  implicit def unitVal[T <: Val](x: T)(implicit mf: Manifest[T]): Rep[Val] = Const(x)

  trait Arith extends Sig.Arith[Dom] {
    override def num(n: Int) = (env: Rep[Env]) => unit(NumVal(n))

    override def add(e1: Dom, e2: Dom) = (env: Rep[Env]) => (e1(env), e2(env)) match {
      case (Const(NumVal(n1)), Const(NumVal(n2))) => unit(NumVal(n1 + n2))
    }

    override def mul(e1: Dom, e2: Dom) = (env: Rep[Env]) => (e1(env), e2(env)) match {
      case (Const(NumVal(n1)), Const(NumVal(n2))) => unit(NumVal(n1 * n2))
    }

    override def if0(c: Dom, t: Dom, e: Dom) = (env: Rep[Env]) => c(env) match {
      case Const(NumVal(n)) =>
        if (n == 0)
          t(env)
        else
          e(env)
    }
  }

  trait Fun extends Sig.Fun[Dom] {
    def lookup(env: Rep[Env], x: String) = env(unit(x))
    override def va(x: String) = (env: Rep[Env]) => lookup(env, x)

    override def abs(x: String, e: Dom) = (env: Rep[Env]) => unit(FunVal(x, e, env))

    override def app(e1: Dom, e2: Dom) = (env: Rep[Env]) => (e1(env), e2(env)) match {
      case (Const(FunVal(x, e, eenv)), v) => e(eenv + (unit(x), v))
    }
  }

  trait Fix extends Sig.Fix[Dom] {
    override def fix(e: Dom) = (env: Rep[Env]) => e(env) match {
      case Const(FunVal(x, fe, fenv)) =>
        val rv = RecVal(null)
        val f = fe(fenv + unit(x -> rv))
        f match {
          case Const(fv) => rv.x = fv
        }
        f
    }
  }

  object ArithFunFix extends Arith with Fun with Fix
}

object StagedEvalIR extends StagedEval

trait FunctionCompiler extends CompileScala with FunctionsExp {
//  val IR: FunctionsExp

  override def reset {
//    IR.reset
    super.reset
  }

  override def isPrimitiveType[T](m: Manifest[T]) = super.isPrimitiveType(m)

  def compile[A: Manifest, B: Manifest](e: Rep[A => B]): A => B = {
    val fun = (x: Rep[A]) => doApply(e, x)
    super.compile(fun)
  }
}

object StagedEvalScalaCodeGen extends StagedEval with FunctionCompiler with ScalaCodeGenPkg with ScalaGenMapOps {
  override val codegen = new ScalaCodeGenPkg {
    override val IR: StagedEvalScalaCodeGen.type = StagedEvalScalaCodeGen
  }

  override val IR = StagedEvalIR

  override def reset {
    IR.reset
    super.reset
  }

  override def isPrimitiveType[T](m: Manifest[T]) = super.isPrimitiveType(m)

  def compile[E](e: Program): Env => Val = {
    val fun = e.apply(ArithFunFix)
    val f = compile(fun)
    f
  }
}

object Test_staged extends App {
  override def main(args: Array[String]) = {
    println("fact: " + StagedEvalScalaCodeGen.compile(Programs.fact)(Map()))
//    println("fact5: " + Programs.fact5.apply(Eval.ArithFunFix)(Map()))
//
//    println("unbound x: " + Types.ArithFunFix.va("x"))
//    println("fact: " + Programs.fact.apply(Types.ArithFunFix))
//    println("fact5: " + Programs.fact5.apply(Types.ArithFunFix))
  }
}