package benchmark.pcf

import incremental.{TypeChecker, TypeCheckerFactory}
import org.scalameter.DSL
import org.scalameter.api._
import benchmark.ExpGenerator._

import incremental.pcf._
import incremental.Exp._

abstract class IncrementalPerformanceTest(maxHeight: Int) extends PerformanceTest {
  val heights: Gen[Int] = Gen.range("height")(2, maxHeight-2, 2)

  def measureCheckers(maxtree: Exp, heights: Gen[Int]): Unit = {
    measureIncremental("DownUp", (e:Exp) => DownUpCheckerFactory.makeChecker.typecheck(e))(maxtree, heights)
//    measureIncremental("BottomUpSolveEnd", (e:Exp) => BottomUpSolveEndCheckerFactory.makeChecker.typecheck(maxtree))(maxtree, heights)
//    measureIncremental("BottomUpIncrementalSolve", (e:Exp) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker.typecheck(e))(maxtree, heights)
//    measureIncremental("BottomUpEagerSubst", (e:Exp) => BottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(maxtree, heights)
    measureIncremental(s"BottomUpSometimesEagerSubst-10", (e:Exp) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10).typecheck(e))(maxtree, heights)

    //    val thresholds = Gen.exponential("threshold")(10, 10000, 10)
    //    val tupled = Gen.tupled(trees,thresholds)
    //    measureTwith(s"BottomUpSometimesEagerSubst", (e:(Exp,Int)) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(e._2).typecheck(e._1))(tupled)
  }

  def measureIncremental(name: String, check: Exp => _)(maxtree: Exp, heights: Gen[Int]): Unit = {
    measure method (name) in {
      using(heights).
      setUp { h =>
        check(maxtree)

        var e = maxtree
        while (Abs.unapplySeq(e).isDefined)
          e = e.kids(0)
        for (i <- 1 to maxHeight - h - 1)
          e = e.kids(0)
        e.invalidate
      }.
      in { _ => check(maxtree) }
    }
  }

  //  def measureTwith[T](name: String, check: ((Exp,T)) => _)(trees: Gen[(Exp,T)]): Unit = {
  //    measure method (name) in {
  //      using(trees).
  //        setUp { _._1.invalidate }.
  //        in { check }
  //    }
  //  }



  /* ADD */

  performance of "Tree{Add,[1..1]}" in {
    val maxtree = makeBinTree(maxHeight, Add, constantLeaveMaker(Num(1)))
    measureCheckers(maxtree, heights)
  }

  performance of "Tree{Add,[1..n]}" in {
    val maxtree = makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" in {
    val maxtree = Abs('x, makeBinTree(maxHeight, Add, constantLeaveMaker(Var('x))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x.1..x.n]}}" in {
    val maxtree = Abs('x, makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => if(i%2==0) Var('x) else Num(i))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x1.1..xn.n]}}" in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => if(i%2==0) Var(Symbol(s"x$i")) else Num(i))))
    measureCheckers(maxtree, heights)
  }



  /* APP */

  performance of "Tree{App,[1..1]}" in {
    val maxtree = makeBinTree(maxHeight, App, constantLeaveMaker(Num(1)))
    measureCheckers(maxtree, heights)
  }

  performance of "Tree{App,[1..n]}" in {
    val maxtree = makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" in {
    val maxtree = Abs('x, makeBinTree(maxHeight, App, constantLeaveMaker(Var('x))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x.1..x.n]}}" in {
    val maxtree = Abs('x, makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => if (i%2==0) Var('x) else Num(i))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x1.1..xn.n]}}" in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => if(i%2==0) Var(Symbol(s"x$i")) else Num(i))))
    measureCheckers(maxtree, heights)
  }
}




object Incremental {
  def main(args: Array[String]): Unit = {
    if (args.size != 2)
      throw new IllegalArgumentException("Expected arguments: (report|micro) maxHeight")

    val kind = args(0).toLowerCase
    val maxHeight = args(1).toInt

    val scalameterArgs = Array("-CresultDir", "./benchmark/incremental")

    if (kind == "report" || kind == "offlinereport")
      new IncrementalOfflineReport(maxHeight).main(scalameterArgs)
    else if (kind == "micro" || kind == "microbenchmark")
      new IncrementalMicroBenchmark(maxHeight).main(scalameterArgs)
  }
}

class IncrementalMicroBenchmark(maxHeight: Int)
  extends IncrementalPerformanceTest(maxHeight)
  with PerformanceTest.Quickbenchmark

class IncrementalOfflineReport(maxHeight: Int)
  extends IncrementalPerformanceTest(maxHeight)
  with PerformanceTest.OfflineReport
