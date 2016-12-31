package benchmark.fjava

import constraints.fjava.impl.{SolveContinuousSubst, SolveEnd}
import incremental.Node.Node
import incremental.Util
import incremental.fjava.{DUCheckerFactory, TypeChecker, earlymerge}
import incremental.fjava.latemerge.BUCheckerFactory

/**
  * Created by seba on 21.12.16.
  */
object Checkers extends App {

  lazy val du: TypeChecker[_] = new DUCheckerFactory(SolveEnd).makeChecker

  lazy val buEnd: TypeChecker[_] = new BUCheckerFactory(SolveEnd).makeChecker
  lazy val buCont: TypeChecker[_] = new BUCheckerFactory(SolveContinuousSubst).makeChecker

  lazy val buEarlyCont: TypeChecker[_] = new earlymerge.BUCheckerFactory(SolveContinuousSubst).makeChecker

  val prog = Trees.intAcumPrevSuperHierarchy(2, 5, 2)(Trees.Unique)

  println("DU check: " + du.typecheck(prog))
  du.localState.printStatistics()

  println("BU check: " + buEarlyCont.typecheck(prog))
  buEarlyCont.localState.printStatistics()
}
