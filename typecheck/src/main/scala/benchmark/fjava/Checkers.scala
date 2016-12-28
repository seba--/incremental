package benchmark.fjava

import constraints.fjava.impl.{SolveContinuousSubst, SolveEnd}
import incremental.Node.Node
import incremental.fjava.DUCheckerFactory
import incremental.fjava.latemerge.BUCheckerFactory

/**
  * Created by seba on 21.12.16.
  */
object Checkers extends App {

  val du = (e:Node) => new DUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
  val buEnd = (e:Node) => new BUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
  val buCont = (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e)

  val prog = Trees.intAcumSuperHierarchy(1, 2, 2)(Trees.Unique)

  buCont(prog)
}
