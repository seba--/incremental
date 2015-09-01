package incremental.pcf

import incremental.Node._
import incremental.pcf.EvalWithConstraints._
import org.scalatest.FunSuite

class TestEvalWithConstraints extends FunSuite {

  def testEval(desc: String, e: => Node)(v: Val) =
    test(desc) {
      val res = eval(e)
      if (res.isLeft) {
        val resVal = res.left.get
        assert(v == resVal)
      }
      else
        fail(s"Evaluation failed with ${res.right.get}, but expected $v")
    }

  testEval("2+3", Add(Num(2), Num(3)))(VNum(5))

  testEval("x => x+3", Abs('x, Add(Var('x), Num(3))))(VNum(5))

  testEval("(x => x+3) 2", App(Abs('x, Add(Var('x), Num(3))), Num(2)))(VNum(5))

  testEval("x => (x+1)+2", Abs('x, Add(Add(Var('x), Num(1)), Num(2))))(VNum(5))

  testEval("(x => (x+1)+2) 2", App(Abs('x, Add(Add(Var('x), Num(1)), Num(2))), Num(2)))(VNum(5))

  testEval("x => x+(1+2)", Abs('x, Add(Var('x), Add(Num(1), Num(2)))))(VNum(5))

  testEval("(x => x+(1+2)) 2", App(Abs('x, Add(Var('x), Add(Num(1), Num(2)))), Num(2)))(VNum(5))
}
