package benchmark.pcf

import incremental.{TypeChecker, TypeCheckerFactory}
import org.scalameter.DSL
import org.scalameter.api._
import benchmark.ExpGenerator._

import incremental.pcf._
import incremental.Exp._

abstract class NonincrementalPerformanceTest(maxHeight: Int) extends PerformanceTest {


  val heights: Gen[Int] = Gen.range("height")(4, maxHeight, 4)

  def measureCheckers(trees: Gen[Exp]): Unit = {
    measureT("DownUp", (e:Exp) => DownUpCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BottomUpSolveEnd", (e:Exp) => BottomUpSolveEndCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BottomUpIncrementalSolve", (e:Exp) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT("BottomUpEagerSubst", (e:Exp) => BottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
    measureT(s"BottomUpSometimesEagerSubst-10", (e:Exp) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10).typecheck(e))(trees)

//    val thresholds = Gen.exponential("threshold")(10, 10000, 10)
//    val tupled = Gen.tupled(trees,thresholds)
//    measureTwith(s"BottomUpSometimesEagerSubst", (e:(Exp,Int)) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(e._2).typecheck(e._1))(tupled)
  }

  def measureT(name: String, check: Exp => _)(trees: Gen[Exp]): Unit = {
    measure method (name) in {
      using(trees).
      setUp { _.invalidate }.
      in { check }
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




  /* APP */

  performance of "Tree{App,[1..n]}" in {
    val trees: Gen[Exp] = for {
      height <- heights
    } yield makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

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
