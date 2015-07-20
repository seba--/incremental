package experiment

import scala.language.implicitConversions

import scala.virtualization.lms.common._

trait Staged extends BaseExp with MapOpsExp {
  type Env = Map[String, Val]
  type Dom = Rep[Env] => Rep[Val]

  trait Val
  case class NumVal(n: Int) extends Val
  case class FunVal(x: String, e: Dom, env: Rep[Env]) extends Val {
    override def toString = s"FunVal($x, $e, _env_)"
  }
  case class RecVal(var x: Val) extends Val

  implicit def unitVal[T <: Val](x: T)(implicit mf: Manifest[T]): Rep[Val] = Const(x)

  trait Arith extends Sig.Arith[Dom] {
    override def num(n: Int) = _ => NumVal(n)

    override def add(e1: Dom, e2: Dom) = env => (e1(env), e2(env)) match {
      case (Const(NumVal(n1)), Const(NumVal(n2))) => NumVal(n1 + n2)
    }

    override def mul(e1: Dom, e2: Dom) = env => (e1(env), e2(env)) match {
      case (Const(NumVal(n1)), Const(NumVal(n2))) => NumVal(n1 * n2)
    }

    override def if0(c: Dom, t: Dom, e: Dom) = env => c(env) match {
      case Const(NumVal(n)) =>
        if (n == 0)
          t(env)
        else
          e(env)
    }
  }

  trait Fun extends Sig.Fun[Dom] {
    def lookup(env: Rep[Env], x: String) = env(unit(x))
    override def va(x: String) = env => lookup(env, x)

    override def abs(x: String, e: Dom) = env => FunVal(x, e, env)

    override def app(e1: Dom, e2: Dom) = env => (e1(env), e2(env)) match {
      case (Const(FunVal(x, e, eenv)), v) => e(eenv + (unit(x), v))
    }
  }

  trait Fix extends Sig.Fix[Dom] {
    override def fix(e: Dom) = env => e(env) match {
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



object Test_staged extends App {
  override def main(args: Array[String]) = {
    println("fact: " + Programs.fact(Eval.ArithFunFix)(Map()))
    println("fact5: " + Programs.fact5(Eval.ArithFunFix)(Map()))

    println("unbound x: " + Types.ArithFunFix.va("x"))
    println("fact: " + Programs.fact(Types.ArithFunFix))
    println("fact5: " + Programs.fact5(Types.ArithFunFix))
  }
}