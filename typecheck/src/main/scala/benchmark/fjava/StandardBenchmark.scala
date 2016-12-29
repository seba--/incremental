package benchmark.fjava

import org.scalameter.api._
import _root_.benchmark.pcf.Settings
import incremental.Node.Node
import Checkers._

/**
  * Standard execution, not parallel, not incremental
  */
class StandardBenchmarkClass extends Bench.LocalTime {

  val opts = org.scalameter.api.Context(
    exec.jvmflags -> (List("-server", "-XX:CompileThreshold=100") ++ Settings("jvmopts"))
  )

//  override def reporter: Reporter[Double] = Reporter.Composite(
//    new RegressionReporter(tester, historian),
//    DsvReporter(' '),
//    HtmlReporter(!online)
//  )

  def measureT(name: String, check: Node => _)(trees: Gen[(Node, Long)]): Unit = {
    measure method (name) in {
      using(trees).
        setUp { _._1.invalidate }.
        in { case (tree, size) => check(tree) }
    }
  }

  def measureCheckers(trees: Gen[(Node, Long)]): Unit = {
    measureT("DU", du)(trees)
//    measureT("BU-End", buEnd)(trees)
//    measureT("BU-Cont", buCont)(trees)
    measureT("BU-Early-Cont", buEarlyCont)(trees)
  }

  measureCheckers(Trees.intAcumSuperHierarchyTrees)
//  measureCheckers(Trees.intAcumPrevHierarchyTrees)
//  measureCheckers(Trees.intAcumPrevSuperHierarchyTrees)
}

object StandardBenchmark {
  def main(args: Array[String]): Unit = {
    val scalameterArgs = Array("-CresultDir", "./benchmark/fjava")
    new StandardBenchmarkClass().main(scalameterArgs)
  }
}

