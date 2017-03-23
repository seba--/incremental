package benchmark.fjava

import org.scalameter.api._
import _root_.benchmark.pcf.Settings
import incremental.Node.Node
import Checkers._
import incremental.fjava.TypeChecker
import org.scalameter.Measurer
import constraints.fjava.ConstraintSystem

/**
  * Standard execution, not parallel, not incremental
  */
class IncrementalBenchmarkClass(memory: Boolean) extends Bench.OfflineReport {

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

  var lastTree: Node = _
  def measureT(name: String, checker: TypeChecker[_])(trees: Gen[(Node, Long)]): Unit = {
    measure method (name) in {
      using(trees).
        setUp { case (tree, size) =>
          lastTree = tree
          checker.typecheck(tree)
          tree.kids(1).kids(0).invalidate
        }.tearDown { case (tree, size) =>
          var reqSize = 0
          var csSize = 0
          tree.visitAll { n =>
            reqSize += n.typ.asInstanceOf[buEarlyCont.Result]._2.size + n.typ.asInstanceOf[buEarlyCont.Result]._3.size
            csSize += n.typ.asInstanceOf[buEarlyCont.Result]._4.asInstanceOf[ConstraintSystem[_]].size
          }
          println(s"Total req size = $reqSize")
          println(s"Total cs size = $csSize")
        }.
        in { case (tree, size) => checker.typecheck(tree) }

    }
  }

  def measureCheckers(trees: Gen[(Node, Long)]): Unit = {
//    measureT("DU", du)(trees)
//    measureT("BU-End", buEnd)(trees)
//    measureT("BU-Cont", buCont)(trees)
    measureT("BU-Early-Cont", buEarlyCont)(trees)
  }

  measureCheckers(Trees.intAccumSuperHierarchyTrees)
  measureCheckers(Trees.intAccumPrevHierarchyTrees)
  measureCheckers(Trees.intAccumPrevSuperHierarchyTrees)
}

object IncrementalBenchmark {
  def main(args: Array[String]): Unit = {
    val memory = if (args.size > 0) args(0) == "memory" else false
    if (memory) println(s"Measuring memory footprint") else println("Measuring wallclock runtime")
    val memsuffix = if (memory) "-memory" else ""
    val scalameterArgs = Array("-CresultDir", "./benchmark/fjava-incremental" + memsuffix)
    new IncrementalBenchmarkClass(memory).main(scalameterArgs)
  }
}

