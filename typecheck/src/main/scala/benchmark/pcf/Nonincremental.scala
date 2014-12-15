package benchmark.pcf

import incremental.{TypeChecker, TypeCheckerFactory}
import org.scalameter.DSL
import org.scalameter.api._
import benchmark.ExpGenerator._

import incremental.pcf._
import incremental.Exp._

abstract class NonincrementalPerformanceTest(maxHeight: Int) extends PerformanceTest {
  val heights: Gen[Int] = Gen.range("height")(2, maxHeight, 2)

  def measureCheckers(trees: Gen[Exp]): Unit = {
    measureT("DownUp", (e:Exp) => DownUpCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BottomUpSolveEnd", (e:Exp) => BottomUpSolveEndCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BottomUpIncrementalSolve", (e:Exp) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BottomUpEagerSubst", (e:Exp) => BottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
  }

  def measureT(name: String, check: Exp => _)(trees: Gen[Exp]): Unit = {
    measure method (name) in {
      using(trees).
      setUp { _.invalidate }.
      in { check }
    }
  }

  performance of "Tree{Add,[1..1]}" in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield makeBinTree(height, Add, constantLeaveMaker(Num(1)))

    measureCheckers(trees)
  }

  performance of "Tree{Add,[1..n]}" in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, Add, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, App, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }
}




object Nonincremental {
  def main(args: Array[String]): Unit = {
    val kind = if (args.size > 0) args(0).toLowerCase else "report"
    val maxHeight = if (args.size > 1) args(1).toInt else 16

    val scalameterArgs = Array("-CresultDir", "./benchmark")

    if (kind == "report" || kind == "offlinereport")
      new NonincrementalOfflineReport(maxHeight).main(scalameterArgs)
    else if (kind == "micro" || kind == "microbenchmark")
      new NonincrementalMicroBenchmark(maxHeight).main(scalameterArgs)
  }
}

class NonincrementalMicroBenchmark(maxHeight: Int)
  extends NonincrementalPerformanceTest(maxHeight)
  with PerformanceTest.Microbenchmark

class NonincrementalOfflineReport(maxHeight: Int)
  extends NonincrementalPerformanceTest(maxHeight)
  with PerformanceTest.OfflineReport
