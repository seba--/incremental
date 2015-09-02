package incremental.pcf

import incremental.Node._
import incremental.pcf.EvalWithConstraints._
import org.scalatest.FunSuite


class TestEvalWithConstraints extends FunSuite {
  import incremental.pcf.EvalWithConstraints.UVar

  def testEval(desc: String, e: => Node)(v: Val) =
    test(desc) {
      val res = eval(e)
      if (res.isLeft) {
        val r = res.left.get
        assert(v == r)
      }
      else
        fail(s"Evaluation failed with ${res.right.get}, but expected $v")
    }
  def testEvalCheck(desc: String, e: => Node)(check: Val => Option[String]) =
    test(desc) {
      val res = eval(e)
      if (res.isLeft) {
        val resVal = res.left.get
        check(resVal) match {
          case None =>
          case Some(s) => fail(s)
        }
      }
      else
        fail(s"Evaluation failed with ${res.right.get}")
    }

  testEval("2+3", Add(Num(2), Num(3)))(VNum(5))

  testEval("x => x+3", Abs('x, Add(Var('x), Num(3)))){
    val vx = UVar('v1)
    val vres = UVar('v2)
    val add = VAdd(vres)(vx, VNum(3))
    VFun(vx, vres, CEmpty :+ add)
  }

  testEval("(x => x+3) 2", App(Abs('x, Add(Var('x), Num(3))), Num(2)))(VNum(5))

  testEval("x => (x+1)+2", Abs('x, Add(Add(Var('x), Num(1)), Num(2)))){
    val vx = UVar('v6)
    val vres = UVar('v8)
    val vres1 = UVar('v7)
    val add2 = VAdd(vres)(vres1, VNum(2))
    val add1 = VAdd(vres1)(vx, VNum(1))
    VFun(vx, vres, ((CEmpty :+ add1) ++ CEmpty ++ CEmpty) :+ add2)
  }

  testEval("(x => (x+1)+2) 2", App(Abs('x, Add(Add(Var('x), Num(1)), Num(2))), Num(2)))(VNum(5))

  testEval("x => x+(1+2)", Abs('x, Add(Var('x), Add(Num(1), Num(2))))){
    val vx = UVar('v13)
    val vres = UVar('v15)
    val add = VAdd(vres)(vx, VNum(3))
    VFun(vx, vres, CEmpty :+ add)
  }

  testEval("(x => x+(1+2)) 2", App(Abs('x, Add(Var('x), Add(Num(1), Num(2)))), Num(2)))(VNum(5))
}
