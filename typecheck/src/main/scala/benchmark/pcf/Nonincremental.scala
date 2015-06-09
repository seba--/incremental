package benchmark.pcf

import incremental.{TypeChecker, TypeCheckerFactory}
import org.scalameter.DSL
import org.scalameter.api._
import benchmark.ExpGenerator._

import incremental.pcf._
import incremental.Exp._

abstract class NonincrementalPerformanceTest(maxHeight: Int) extends PerformanceTest {
  val opts = org.scalameter.api.Context(
    exec.jvmflags -> ("-server -XX:CompileThreshold=100 " + Settings("jvmopts"))
  )

  val heights: Gen[Int] = Gen.range("height")(2, maxHeight, 2)

  def measureCheckers(trees: Gen[Exp]): Unit = {
    measureT("DU", (e:Exp) => DownUpCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BU1", (e:Exp) => BottomUpSolveEndCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BU2", (e:Exp) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(Int.MaxValue).typecheck(e))(trees)
    measureT("BU3", (e:Exp) => BottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BU4", (e:Exp) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10).typecheck(e))(trees)
  }

  def measureT(name: String, check: Exp => _)(trees: Gen[Exp]): Unit = {
    measure method (name) in {
      using(trees).
      setUp { _.invalidate }.
      in { check }
    }
  }

  /* ADD */

  performance of "Tree{Add,[1..n]}" config (opts) in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" config (opts) in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, Add, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" config (opts) in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }




  /* APP */

  performance of "Tree{App,[1..n]}" config (opts) in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" config (opts) in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, App, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" config (opts) in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }
}




object Nonincremental {
  def main(args: Array[String]): Unit = {
    if (args.size != 2)
      throw new IllegalArgumentException("Expected arguments: (report|micro) maxHeight")

    val kind = args(0).toLowerCase
    val maxHeight = args(1).toInt

    val scalameterArgs = Array("-CresultDir", "./benchmark/nonincremental")

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
  with PerformanceTest.OfflineReport {

  override def reporter: Reporter = Reporter.Composite(
    new RegressionReporter(tester, historian),
    DsvReporter(' '),
    HtmlReporter(!online)
  )
}
