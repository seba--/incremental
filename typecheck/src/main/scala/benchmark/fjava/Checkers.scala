package benchmark.fjava

import constraints.fjava.impl.{SolveContinuousSubst, SolveEnd}
import incremental.Node.Node
import incremental.fjava.{BUCheckerFactory, DUCheckerFactory}

/**
  * Created by seba on 21.12.16.
  */
object Checkers {

  val du = (e:Node) => new DUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
  val buEnd = (e:Node) => new BUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
  val buCont = (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e)

}
