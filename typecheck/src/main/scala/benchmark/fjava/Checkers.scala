package benchmark.fjava

import constraints.fjava.impl.{SolveContinuousSubst, SolveEnd}
import incremental.Node.Node
import incremental.Util
import incremental.fjava.{DUCheckerFactory, earlymerge}
import incremental.fjava.latemerge.BUCheckerFactory

/**
  * Created by seba on 21.12.16.
  */
object Checkers extends App {

  def du = (e:Node) => new DUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
  def buEnd = (e:Node) => new BUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
  def buCont = (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e)

  def buEarlyCont = (e:Node) => new earlymerge.BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e)

  val prog = Trees.intAcumSuperHierarchy(10, 5, 2)(Trees.Unique)

  println(Util.timed(du(prog)))
  println(Util.timed(buEarlyCont(prog)))
}
