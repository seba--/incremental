package benchmark.fjava

import constraints.Statistics
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

  val roots = 40
  val height = 5
  val prog = Trees.intAcumPrevHierarchy(roots, height, 2)(Trees.Mirrored)

//  println(prog)

  println("DU check: " + du.typecheck(prog))
  du.localState.printStatistics()

//  println("DUCont check: " + duCont.typecheck(prog))
//  duCont.localState.printStatistics()

  println("BU check: " + buEarlyCont.typecheck(prog))
  buEarlyCont.localState.printStatistics()

  println()
  var count = 0
  var time = 0.0
  for (i <- 1 to 3) {
    count += 1

    prog.kids(1).kids(0).invalidate
    buEarlyCont.typecheck(prog)
    time += buEarlyCont.localState.stats(Statistics.typecheckTime)
    buEarlyCont.localState.printStatistics()
    println("---")
  }

  println(s"BU check incremental (average of $count runs): " + (time / count))

}
