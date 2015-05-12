package benchmark.pcf

import constraints.equality.impl.SolveContinuousSubst
import incremental.pcf.Exp.Exp
import incremental.{TypeChecker, TypeCheckerFactory}
import org.scalameter.DSL
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

    measureT("BUSolveContinuousSubst", (e:Node) => new BUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticBUSolveContinuousSubst", (e:Node) => new FuturisticBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticHeightBUSolveContinuousSubst", (e:Node) => new FuturisticHeightBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    measureT("FuturisticHeightListBUSolveContinuousSubst", (e:Node) => new FuturisticHeightListBUCheckerFactory(SolveContinuousSubst).makeChecker.typecheck(e))(trees)
    //measureT("FuturisticBottomUpEagerSubst", (e:Node) => FuturisticBottomUpEagerSubstCheckerFactory.makeChecker.typecheck(e))(trees)
   // measureT("BottomUpEagerSubstConcurrent", (e:Node) => BottomUpEagerSubstConcurrentCheckerFactory.makeChecker.typecheck(e))(trees)

   // measureT(s"BottomUpSometimesEagerSubst-10", (e:Node) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(10).typecheck(e))(trees)
//    val thresholds = Gen.exponential("threshold")(10, 10000, 10)
//    val tupled = Gen.tupled(trees,thresholds)
//    measureTwith(s"BottomUpSometimesEagerSubst", (e:(Exp,Int)) => BottomUpSometimesEagerSubstCheckerFactory.makeChecker(e._2).typecheck(e._1))(tupled)
  }

  def measureT(name: String, check: Node => _)(trees: Gen[Node]): Unit = {

    for (params <- trees.dataset) {
      var iterations = 0
      var time = 0.0
      var oks = 0
      var errors= 0

      val tree = trees.generate(params)

      tree.invalidate
      val start = System.nanoTime()
      val res = check(tree)
      res match {
        case Left(_) =>
          oks += 1
        case _ => errors += 1
      }
      val end = System.nanoTime()
      time += (end-start)
      iterations += 1

      val avg = if (iterations == 0) time else time/(1000000.0*iterations)
      println(s"$name ($params): t: ${avg}ms ok: $oks fail: $errors")
    }
  }

  private def performance(name: String)(thunk: => Any): Unit = {
    println("---------------------------------- " + name)
    val x = thunk
    println("===================================")
    println()
    println()
  }

//  def measureTwith[T](name: String, check: ((Exp,T)) => _)(trees: Gen[(Exp,T)]): Unit = {
//    measure method (name) in {
//      using(trees).
//        setUp { _._1.invalidate }.
//        in { check }
//    }
//  }



  /* ADD */

  performance("Tree{Add,[1..n]}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield makeBinTree[Exp](height, Add.apply(_,_), stateLeaveMaker[Int,Exp](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance("Abs{x,Tree{Add,[x..x]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree[Exp](height, Add.apply(_,_), constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance("Abs{x,Tree{Add,[x1..xn]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield AbsMany(usedVars(height), makeBinTree[Exp](height, Add.apply(_,_), stateLeaveMaker[Int,Exp](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }




  /* APP */

  performance("Tree{App,[1..n]}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield makeBinTree[Exp](height, App.apply(_,_), stateLeaveMaker[Int,Exp](1, i => i + 1, i => Num(i)))

    measureCheckers(trees)
  }

  performance("Abs{x,Tree{App,[x..x]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield Abs('x, makeBinTree[Exp](height, App.apply(_,_), constantLeaveMaker(Var('x))))

    measureCheckers(trees)
  }

  performance("Abs{x,Tree{App,[x1..xn]}}") {
    val trees: Gen[Node] = for {
      height <- heights
    } yield AbsMany(usedVars(height), makeBinTree[Exp](height, App.apply(_,_), stateLeaveMaker[Int,Exp](1, i => i + 1, i => Var(Symbol(s"x$i")))))

    measureCheckers(trees)
  }
}




object NonincrementalLightweight {
  def main(args: Array[String]): Unit = {
    if (args.size != 1)
      throw new IllegalArgumentException("Expected arguments: maxHeight")

    val maxHeight = args(0).toInt


    println("prepare profiler, then hit enter")
    StdIn.readLine()
    new LightweightPerformanceTest(maxHeight)
  }
}

