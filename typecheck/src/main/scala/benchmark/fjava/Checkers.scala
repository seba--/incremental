package benchmark.fjava

import constraints.Statistics
import constraints.fjava.Type
import constraints.fjava.impl._
import incremental.Node.Node
import incremental.fjava._
import incremental.fjava.latemerge.BUCheckerFactory

/**
  * Created by seba on 21.12.16.
  */
object Checkers extends App {

  lazy val du: TypeChecker[_] = new DUCheckerFactory(SolveEnd).makeChecker
  lazy val duCont: TypeChecker[_] = new DUCheckerFactory(SolveContinuousSubstLateMerge).makeChecker

  lazy val buEnd: TypeChecker[_] = new BUCheckerFactory(SolveEnd).makeChecker
  lazy val buCont: TypeChecker[_] = new BUCheckerFactory(SolveContinuousSubstLateMerge).makeChecker

  lazy val buEarlyCont: TypeChecker[_] = new earlymerge.BUCheckerFactory(SolveContinuousSubstEarlyMerge).makeChecker

  lazy val javac: TypeChecker[_] = JavacCheckerFactory(SolveEnd).makeChecker
}
