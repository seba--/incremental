package benchmark.pcf

import java.util.concurrent.{ThreadPoolExecutor, Executors}

import constraints.Statistics
import constraints.equality.impl.SolveContinuousSubst
import incremental.pcf.concurrent._
import incremental.{TypeChecker, TypeCheckerFactory}
import org.scalameter.{Parameters, DSL}
import org.scalameter.api._
import benchmark.ExpGenerator._

import incremental.pcf._
import incremental.Node._

import scala.io.StdIn

class LightweightPerformanceTest(maxHeight: Int) {

  val heights: Gen[Int] = Gen.range("height")(2, maxHeight, 2)

  def measureCheckers(trees: Gen[Node]): Unit = {
   // measureT("DownUp", (e:Node) => DownUpCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpSolveEnd", (e:Node) => BottomUpSolveEndCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpIncrementalSolve", (e:Node) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpEagerSubst", (e:Node) => BottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
    println(s"Detected ${Runtime.getRuntime().availableProcessors()} cores")
    baselineTime = 0l
    for (params <- trees.dataset if params[Int]("height") == 16) {
      val tree = trees.generate(params)
   //   measureT("DU", (e: Node) =>  BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
//      measureT("SingleFutureBUSolveContinuousSubst", (e: Node) => SingleFutureBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
  //    measureT("FuturisticHeight", (e: Node) => FuturisticHeightBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
//      measureT("FuturisticLevelBUSolveContinuousSubst", (e: Node) => FuturisticLevelBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
//      measureT("FuturisticBUSolveContinuousSubst", (e: Node) => FuturisticBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
   //   measureT("JoinHeight", (e: Node) => JoinBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
      //measureT("Join2Height", (e: Node) => Join2BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
      //measureT("Join3Height", (e: Node) => Join3BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(params, tree)
      measureT("Seq", new SequentialChecker)(params, tree)
     // for (i <- 0 to 6) {
   //   measureT(s"WSJoin(3)", (e: Node) => (new WorkStealingChecker(3)).typecheckImpl(e))(params, tree)

      for (i <- 0 to 6)
        measureT(s"ThreadJoin($i)", new ThreadChecker(i))(params, tree)

      //}
      val optSpeedup = optimalSpeedup(tree)
      println(s"Optimal speedup: $optSpeedup")
      println()
    }
    //measureT("FuturisticBottomUpEagerSubst", (e:Node) => FuturisticBottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpEagerSubstConcurrent", (e:Node) => BottomUpEagerSubstConcurrentCheckerFactory.makeChecker.typecheck(e))(trees)

   // measureT(s"BottomUpSometimesEagerSubst-10", (e:Node) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10).typecheck(e))(trees)
//    val thresholds = Gen.exponential("threshold")(10, 10000, 10)
//    val tupled = Gen.tupled(trees,thresholds)
//    measureTwith(s"BottomUpSometimesEagerSubst", (e:(Exp,Int)) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(e._2).typecheck(e._1))(tupled)
  }

  val baselineName = "Seq"
  var baselineTime = 0l

  def measureT(name: String, mkChecker: => LightweightChecker.Checker)(params: Parameters, tree: Node): Unit = {
   // System.gc()
   // Thread.sleep(2000)
    var time = 0l
    var oks = 0
    var errors= 0
    val checker = mkChecker
    val iterations = 120

    for(i <- 0 until iterations) {
      tree.invalidate
      val start = System.nanoTime()
      val res = checker.typecheckImpl(tree)
      res match {
        case Left(_) =>
          oks += 1
        case _ => errors += 1
      }
      val end = System.nanoTime()
      time += (end - start)



    }
    val avgnano = time/iterations
    val avg = avgnano/1000000.0
    if (name == baselineName)
      baselineTime = time

    println(f"$name (${params("height")}):\nt: $avg%2.2fms ok: $oks fail: $errors speedup: ${baselineTime.toDouble/time}%2.2f")
  }

  private def performance(name: String)(thunk: => Any): Unit = {
    println("---------------------------------- " + name)
    val x = thunk
    println("===================================")
    println()
    println()
  }

  def optimalSpeedup(tree: Node): Double = {
    import scala.math._
    val numCores = Runtime.getRuntime().availableProcessors()
    val layers = Array.fill(tree.height + 1)(0)
    tree.foreach[Unit] { node =>
      layers(node.height) = layers(node.height) + 1
    }

    val steps = layers.foldLeft(0d) { case (sum, n) => sum + ceil(n.toDouble/numCores)   }

    tree.size / steps
  }

//  def measureTwith[T](name: String, check: ((Exp,T)) => _)(trees: Gen[(Exp,T)]): Unit = {
//    measure method (name) in {
//      using(trees).
//        setUp { _._1.invalidate }.
//        in { check }
//    }
//  }



//  /* ADD */

// performance("Tree{Add,[1..n]}") {
//    val trees: Gen[Node] = for {
//      height <- heights
//    } yield makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))
//
//    measureCheckers(trees)
//  }

  performance("Abs{x,Tree{Add,[x..x]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, Add, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance("Abs{x,Tree{Add,[x1..xn]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, Add, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }
//
//
//
//
//  /* APP */

  performance("Tree{App,[1..n]}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance("Abs{x,Tree{App,[x..x]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree(height, App, constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance("Abs{x,Tree{App,[x1..xn]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs(usedVars(height), makeBinTree(height, App, stateLeaveMaker[Int](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }
}




object NonincrementalLightweight {
  def main(args: Array[String]): Unit = {
    if (args.size < 1)
      throw new IllegalArgumentException("Expected arguments: maxHeight [stats]")

    val maxHeight = args(0).toInt

    Statistics.ENABLED = false

    println("prepare profiler, then hit enter")
    StdIn.readLine()
    new LightweightPerformanceTest(maxHeight)
    JoinBUChecker.pool.shutdown()
  }
}

