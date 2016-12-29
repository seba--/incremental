package benchmark.pcf

import org.scalameter.api._
import _root_.benchmark.ExpGenerator._
import constraints.equality.impl._
import incremental.pcf._
import incremental.Node._

import org.scalameter.picklers.Implicits._

class IncrementalOfflineReport(maxHeight: Int) extends Bench.OfflineReport {
  val opts = org.scalameter.api.Context(
    exec.jvmflags -> (List("-server", "-XX:CompileThreshold=100") ++ Settings("jvmopts"))
  )

  override def reporter: Reporter[Double] = Reporter.Composite(
    new RegressionReporter(tester, historian),
    DsvReporter(' '),
    HtmlReporter(!online)
  )

  val heights: Gen[Int] = Gen.enumeration("height")(Seq(2, maxHeight/2, maxHeight).distinct:_*)

  def measureCheckers(maxtree: Node, heights: Gen[Int]): Unit = {
//    def du = DownUpCheckerFactory.makeChecker
// Due to timeouts, we excluded BU1 from this experiment
// val bu1 = BottomUpSolveEndCheckerFactory.makeChecker
//    def bu2 = BottomUpSometimesEagerSubstCheckerFactory.makeChecker(Int.MaxValue)
//    def bu3 = BottomUpEagerSubstCheckerFactory.makeChecker
//    def bu4 = BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10)

    val du = (e:Node) => new DUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
    val bu1 = (e:Node) => new BUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
    val bu2 = (e:Node) => new BUCheckerFactory(SolveContinuously).makeChecker.typecheck(e)
    val bu3 = (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e)
    val bu4 = (e:Node) => new BUCheckerFactory(SolveContinuousSubstThreshold).makeChecker.typecheck(e)

//    measureIncremental("DU", (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(maxtree, heights)
    //BU1 is very slow for heights bigger than 12, therefore we exclude it
    if (maxHeight <= 12)
      measureIncremental("BU1", bu1)(maxtree, heights)
    measureIncremental("BU2", bu2)(maxtree, heights)
    measureIncremental("BU3", bu3)(maxtree, heights)
    measureIncremental("BU4", bu4)(maxtree, heights)
  }

  def measureIncremental(name: String, check: Node => _)(maxtree: Node, heights: Gen[Int]): Unit = {
    var firstime = true
    measure method (name) in {
      using(heights).
        setUp { h =>
        if (firstime) {
          check(maxtree)
          firstime = false
        }

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


  /* ADD */

  performance of "Tree{Add,[1..n]}" config (opts) in {
    val maxtree = makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" config (opts) in {
    val maxtree = Abs('x, makeBinTree(maxHeight, Add, constantLeaveMaker(Var('x))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" config (opts) in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))
    measureCheckers(maxtree, heights)
  }



//  /* APP */

  performance of "Tree{App,[1..n]}" config (opts) in {
    val maxtree = makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" config (opts) in {
    val maxtree = Abs('x, makeBinTree(maxHeight, App, constantLeaveMaker(Var('x))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" config (opts) in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))
    measureCheckers(maxtree, heights)
  }
}


class IncrementalMicroBenchmark(maxHeight: Int) extends Bench.LocalTime {
  val opts = org.scalameter.api.Context(
    exec.jvmflags -> (List("-server", "-XX:CompileThreshold=100") ++ Settings("jvmopts"))
  )

  val heights: Gen[Int] = Gen.range("height")(2, maxHeight, 2)

  def measureCheckers(maxtree: Node, heights: Gen[Int]): Unit = {
    val du = (e:Node) => new DUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
    val bu1 = (e:Node) => new BUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
    val bu2 = (e:Node) => new BUCheckerFactory(SolveContinuously).makeChecker.typecheck(e)
    val bu3 = (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e)
    val bu4 = (e:Node) => new BUCheckerFactory(SolveContinuousSubstThreshold).makeChecker.typecheck(e)

    measureIncremental("DU", (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(maxtree, heights)
    //BU1 is very slow for heights bigger than 12, therefore we exclude it
    if (maxHeight <= 12)
      measureIncremental("BU1", bu1)(maxtree, heights)
    measureIncremental("BU2", bu2)(maxtree, heights)
    measureIncremental("BU3", bu3)(maxtree, heights)
    measureIncremental("BU4", bu4)(maxtree, heights)
  }

  def measureIncremental(name: String, check: Node => _)(maxtree: Node, heights: Gen[Int]): Unit = {
    var firstime = true
    measure method (name) in {
      using(heights).
        setUp { h =>
          if (firstime) {
            check(maxtree)
            firstime = false
          }

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


  /* ADD */

  performance of "Tree{Add,[1..n]}" config (opts) in {
    val maxtree = makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" config (opts) in {
    val maxtree = Abs('x, makeBinTree(maxHeight, Add, constantLeaveMaker(Var('x))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" config (opts) in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))
    measureCheckers(maxtree, heights)
  }



  //  /* APP */

  performance of "Tree{App,[1..n]}" config (opts) in {
    val maxtree = makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" config (opts) in {
    val maxtree = Abs('x, makeBinTree(maxHeight, App, constantLeaveMaker(Var('x))))
    measureCheckers(maxtree, heights)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" config (opts) in {
    val maxtree = Abs(usedVars(maxHeight), makeBinTree(maxHeight, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))
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

