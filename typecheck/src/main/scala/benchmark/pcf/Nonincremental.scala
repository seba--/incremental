package benchmark.pcf

import constraints.equality.impl._
import org.scalameter.api._
import _root_.benchmark.ExpGenerator._

import scala.io.StdIn
import incremental.pcf._
import incremental.Node._

class NonincrementalOfflineReport(maxHeight: Int) extends Bench.OfflineReport {
  val opts = org.scalameter.api.Context(
    exec.jvmflags -> List("-server", "-XX:CompileThreshold=100", Settings("jvmopts"))
  )

  override def reporter: Reporter[Double] = Reporter.Composite(
    new RegressionReporter(tester, historian),
    DsvReporter(' '),
    HtmlReporter(!online)
  )

  val heights: Gen[Int] = Gen.range("height")(2, maxHeight, 2)

  def measureCheckers(trees: Gen[Node]): Unit = {
    val du = (e:Node) => new DUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
    val bu1 = (e:Node) => new BUCheckerFactory(SolveEnd).makeChecker.typecheck(e)
    val bu2 = (e:Node) => new BUCheckerFactory(SolveContinuously).makeChecker.typecheck(e)
    val bu3 = (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e)
    val bu4 = (e:Node) => new BUCheckerFactory(SolveContinuousSubstThreshold).makeChecker.typecheck(e)

    measureT("DU", du)(trees)
    
    //BU1 is very slow for heights bigger than 12, therefore we exclude it
    if (maxHeight <= 12)
      measureT("BU1", bu1)(trees)
    measureT("BU2", bu2)(trees)
    measureT("BU3", bu3)(trees)
    measureT("BU4", bu4)(trees)


//    measureT("BUSolveContinuousSubst", (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
//    measureT("FuturisticBUSolveContinuousSubst", (e:Node) => new FuturisticBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
//    measureT("FuturisticHeightBUSolveContinuousSubst", (e:Node) => new FuturisticHeightBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
//    measureT("FuturisticHeightListBUSolveContinuousSubst", (e:Node) => new FuturisticHeightListBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
  }

  def measureT(name: String, check: Node => _)(trees: Gen[Node]): Unit = {
    measure method (name) in {
      using(trees).
      setUp { _.invalidate }.
      in { check }
    }
  }

  /* ADD */

  performance of "Tree{Add,[1..n]}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, Add, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }




  /* APP */

  performance of "Tree{App,[1..n]}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, App, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }
}


class NonincrementalMicroBenchmark(maxHeight: Int) extends Bench.LocalTime {
  val opts = org.scalameter.api.Context(
    exec.jvmflags -> List("-server -XX:CompileThreshold=100", Settings("jvmopts"))
  )

  val heights: Gen[Int] = Gen.range("height")(2, maxHeight, 2)

  def measureCheckers(trees: Gen[Node]): Unit = {
    //measureT("DU", (e:Node) => DownUpCheckerFactory.makeChecker.typecheck(e))(trees)

    //BU1 is very slow for heights bigger than 12, therefore we exclude it
    //if (maxHeight <= 12)
    //  measureT("BU1", (e:Node) => BottomUpSolveEndCheckerFactory.makeChecker.typecheck(e))(trees)

    //measureT("BU2", (e:Node) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(Int.MaxValue).typecheck(e))(trees)
    //measureT("BU3", (e:Node) => BottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
    //measureT("BU4", (e:Node) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10).typecheck(e))(trees)
    measureT("BUSolveContinuousSubst", (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticBUSolveContinuousSubst", (e:Node) => new FuturisticBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticHeightBUSolveContinuousSubst", (e:Node) => new FuturisticHeightBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticHeightListBUSolveContinuousSubst", (e:Node) => new FuturisticHeightListBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
  }

  def measureT(name: String, check: Node => _)(trees: Gen[Node]): Unit = {
    measure method (name) in {
      using(trees).
        setUp { _.invalidate }.
        in { check }
    }
  }

  /* ADD */

  performance of "Tree{Add,[1..n]}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x..x]}}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, Add, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{Add,[x1..xn]}}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }




  /* APP */

  performance of "Tree{App,[1..n]}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x..x]}}" config (opts) in {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, App, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance of "Abs{x,Tree{App,[x1..xn]}}" config (opts) in {
    val trees: Gen[Node] = for {
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

    kind match {
      case "report" | "offlinereport" =>
        new NonincrementalOfflineReport(maxHeight).main(scalameterArgs)
      case "micro" | "microbenchmark" =>
        new NonincrementalMicroBenchmark(maxHeight).main(scalameterArgs)
//      case "quick" =>
//        println("prepare your profiler, then hit enter")
//        StdIn.readLine()
//        new NonincrementalQuickBenchmark(maxHeight).main(scalameterArgs)
      case _ =>
        throw new IllegalArgumentException(s"parameter $kind not understood")
    }
  }
}
