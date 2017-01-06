package benchmark.fjava

import constraints.fjava.impl.{SolveContinuousSubst, SolveEnd}
import incremental.fjava.{DUCheckerFactory, TypeChecker, earlymerge}
import incremental.fjava.latemerge.BUCheckerFactory

/**
  * Created by seba on 21.12.16.
  */
object Checkers extends App {

  lazy val du: TypeChecker[_] = new DUCheckerFactory(SolveEnd).makeChecker
  lazy val duCont: TypeChecker[_] = new DUCheckerFactory(SolveContinuousSubst).makeChecker

  lazy val buEnd: TypeChecker[_] = new BUCheckerFactory(SolveEnd).makeChecker
  lazy val buCont: TypeChecker[_] = new BUCheckerFactory(SolveContinuousSubst).makeChecker

  lazy val buEarlyCont: TypeChecker[_] = new earlymerge.BUCheckerFactory(SolveContinuousSubst).makeChecker

  val prog = Trees.intAcumSuperHierarchy(10, 5, 2)(Trees.Unique)

//  println(prog)

  println("DU check: " + du.typecheck(prog))
  du.localState.printStatistics()

//  println("DUCont check: " + duCont.typecheck(prog))
//  duCont.localState.printStatistics()

  println("BU check: " + buEarlyCont.typecheck(prog))
  buEarlyCont.localState.printStatistics()
}
