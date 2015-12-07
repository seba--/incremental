package benchmark.pcf

import constraints.equality.Constraint
import constraints.equality.impl.SolveContinuousSubst
import incremental.pcf.PCFCheck.Result
import incremental.{TypeChecker, TypeCheckerFactory}
import org.scalameter.DSL
import org.scalameter.api._
import benchmark.ExpGenerator._

import incremental.pcf._
import incremental.Node._

import scala.io.StdIn

abstract class NonincrementalPerformanceTest(maxHeight: Int) extends PerformanceTest {

  val opts = org.scalameter.api.Context(
    exec.jvmflags -> "-server -Xmx4096m -Xms2048m -XX:CompileThreshold=100"
  )

  val heights: Gen[Int] = Gen.range("height")(2, maxHeight, 2)

  def measureCheckers(trees: Gen[Node[Constraint, Result]]): Unit = {
   // measureT("DownUp", (e:Node[Constraint, Result]) => DownUpCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpSolveEnd", (e:Node[Constraint, Result]) => BottomUpSolveEndCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpIncrementalSolve", (e:Node[Constraint, Result]) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpEagerSubst", (e:Node[Constraint, Result]) => BottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)

    //measureT("BUSolveContinuousSubst", (e:Node[Constraint, Result]) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    //measureT("FuturisticBUSolveContinuousSubst", (e:Node[Constraint, Result]) => new FuturisticBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("BUSolveContinuousSubst", (e:Node[Constraint, Result]) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticBUSolveContinuousSubst", (e:Node[Constraint, Result]) => new FuturisticBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticHeightBUSolveContinuousSubst", (e:Node[Constraint, Result]) => new FuturisticHeightBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticHeightListBUSolveContinuousSubst", (e:Node[Constraint, Result]) => new FuturisticHeightListBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    //measureT("FuturisticBottomUpEagerSubst", (e:Node[Constraint, Result]) => FuturisticBottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpEagerSubstConcurrent", (e:Node[Constraint, Result]) => BottomUpEagerSubstConcurrentCheckerFactory.makeChecker.typecheck(e))(trees)

   // measureT(s"BottomUpSometimesEagerSubst-10", (e:Node[Constraint, Result]) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10).typecheck(e))(trees)
//    val thresholds = Gen.exponential("threshold")(10, 10000, 10)
//    val tupled = Gen.tupled(trees,thresholds)
//    measureTwith(s"BottomUpSometimesEagerSubst", (e:(Exp,Int)) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(e._2).typecheck(e._1))(tupled)
  }

  def measureT(name: String, check: Node[Constraint, Result] => _)(trees: Gen[Node[Constraint, Result]]): Unit = {
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

  performance of "Tree{Add,[1..n]}" config (opts) in {
    val trees: Gen[Node[Constraint, Result]] = for {
      height <- heights
    } yield makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" config (opts) in {
    val trees: Gen[Node[Constraint, Result]] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, Add, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" config (opts) in {
    val trees: Gen[Node[Constraint, Result]] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }




  /* APP */

  performance of "Tree{App,[1..n]}" config (opts) in {
    val trees: Gen[Node[Constraint, Result]] = for {
      height <- heights
    } yield makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" config (opts) in {
    val trees: Gen[Node[Constraint, Result]] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, App, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" config (opts) in {
    val trees: Gen[Node[Constraint, Result]] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }
}




object Nonincremental {
  def main(args: Array[String]): Unit = {
    if (args.size != 2)
      throw new IllegalArgumentException("Expected arguments: (report|micro|quick) maxHeight")

    val kind = args(0).toLowerCase
    val maxHeight = args(1).toInt

    val scalameterArgs = Array("-CresultDir", "./benchmark/nonincremental")

    kind match {
      case "report" | "offlinereport" =>
        new NonincrementalOfflineReport(maxHeight).main(scalameterArgs)
      case "micro" | "microbenchmark" =>
        new NonincrementalMicroBenchmark(maxHeight).main(scalameterArgs)
      case "quick" =>
        println("prepare your profiler, then hit enter")
        StdIn.readLine()
        new NonincrementalQuickBenchmark(maxHeight).main(scalameterArgs)
      case _ =>
        throw new IllegalArgumentException(s"parameter $kind not understood")
    }
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

class NonincrementalQuickBenchmark(maxHeight: Int)
  extends NonincrementalPerformanceTest(maxHeight)
  with PerformanceTest.Quickbenchmark