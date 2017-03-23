package benchmark.fjava

import org.scalameter.api._
import _root_.benchmark.pcf.Settings
import incremental.Node.Node
import Checkers._
import incremental.fjava.TypeChecker
import org.scalameter.Measurer

/**
  * Standard execution, not parallel, not incremental
  */
class StandardBenchmarkClass(memory: Boolean) extends Bench.OfflineReport {

  override def measurer: Measurer[Double] =
    if (memory)
      new Measurer.MemoryFootprint with Measurer.OutlierElimination[Double]
    else
      super.measurer

  val opts = org.scalameter.api.Context(
    exec.jvmflags -> (List("-server", "-XX:CompileThreshold=100") ++ Settings("jvmopts"))
  )

  override def reporter: Reporter[Double] = Reporter.Composite(
    new RegressionReporter(tester, historian),
    DsvReporter(' '),
    HtmlReporter(!online)
  )

  def measureT(name: String, checker: TypeChecker[_])(trees: Gen[(Node, Long)]): Unit = {
    measure method (name) in {
      using(trees).
        setUp { _._1.invalidate }.
        in { case (tree, size) => checker.typecheck(tree) }
    }
  }

  def measureCheckers(trees: Gen[(Node, Long)]): Unit = {
    measureT("DU", du)(trees)
//    measureT("BU-End", buEnd)(trees)
//    measureT("BU-Cont", buCont)(trees)
    measureT("BU-Early-Cont", buEarlyCont)(trees)
  }

  measureCheckers(Trees.intAccumSuperHierarchyTrees)
  measureCheckers(Trees.intAccumPrevHierarchyTrees)
  measureCheckers(Trees.intAccumPrevSuperHierarchyTrees)
}

object StandardBenchmark {
  def main(args: Array[String]): Unit = {
    val memory = if (args.size > 0) args(0) == "memory" else false
    if (memory) println(s"Measuring memory footprint") else println("Measuring wallclock runtime")
    val memsuffix = if (memory) "-memory" else ""
    val scalameterArgs = Array("-CresultDir", "./benchmark/fjava-nonincremental" + memsuffix)
    new StandardBenchmarkClass(memory).main(scalameterArgs)
  }
}

