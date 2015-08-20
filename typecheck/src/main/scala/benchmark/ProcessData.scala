package benchmark

import java.io.File
import java.util.regex.Pattern

import scopt.OptionParser

import scala.collection.immutable.{ListMap, TreeMap}

object ProcessData extends App {


  case class Config(
    dir: File,
    isIncrementalData: Boolean,
    progs: Map[String, String], // file name fragment -> latex representation
    checkers: Map[String, String] // file name fragment -> latex representation
  )

  var pcfProgs = ListMap[String, String]()
  pcfProgs += "Tree{Add,[1..n]}" -> "{\\scriptsize$\\treegen{+}{h}{1 \\ldots n}$}"
  pcfProgs += "Abs{x,Tree{Add,[x..x]}}" -> "{\\scriptsize$\\treegen{+}{h}{x \\ldots x}$}"
  pcfProgs += "Abs{x,Tree{Add,[x1..xn]}}" -> "{\\scriptsize$\\treegen{+}{h}{x_1 \\ldots x_n}$}"
  pcfProgs += "Tree{App,[1..n]}" -> "{\\scriptsize$\\treegen{\\textsf{app}}{h}{1 \\ldots n}$}"
  pcfProgs += "Abs{x,Tree{App,[x..x]}}" -> "{\\scriptsize$\\treegen{\\textsf{app}}{h}{x \\ldots x}$}"
  pcfProgs += "Abs{x,Tree{App,[x1..xn]}}" -> "{\\scriptsize$\\treegen{\\textsf{app}}{\\hfake{16}{h}}{x_1 \\ldots x_n}$}"

  var pcfCheckers = ListMap[String, String]()
  pcfCheckers += "DU" -> "DU"
  pcfCheckers += "BU1" -> "BU1"
  pcfCheckers += "BU2" -> "BU2"
  pcfCheckers += "BU3" -> "BU3"
  pcfCheckers += "BU4" -> "BU4"

  val optionParser = new OptionParser[Config]("process-data") {
    head("process evaluation data of co-contextual type checkers")

    opt[Boolean]("incremental") required() action { (inc, config) =>
      config.copy(isIncrementalData = inc)
    }

    arg[File]("<directory>") action { (dir, config) =>
      config.copy(dir = dir)
    } validate { f =>
      if (f.exists()) success else failure(s"directory not found $f")
    }
  }

  optionParser.parse(args, Config(null, false, pcfProgs, pcfCheckers)) match {
    case None =>
    case Some(config) => run(config)
  }

  def run(config: Config): Unit = {

    val files = config.dir.listFiles()

    val rows = config.progs.size + 1
    val cols = config.checkers.size + 1
    val table = Array[Array[String]].ofDim(row, cols) // list of rows

    table(0)(0) = "Tree"
    var i = 1
    for (checker <- config.checkers) {
      table(0)(i) = checker._2
      i += 1
    }

    for (checker <- config.checkers;
         prog <- config.progs) {

      val fileRegex = Pattern.quote(s"${prog._1}.${checker._1}.Test-") + "\\d*" + Pattern.quote(".dsv")
      val mfile = files.find(f => f.getName.matches(fileRegex))

      mfile match {
        case None =>
          println(s"${prog._1}.${checker._1}.Test-*.dsv -> n/a")
        case Some(file) =>
          val avg = processFile(file, config.isIncrementalData, checker, prog)
          println(s"${file.getName} -> $avg")
      }
    }
  }

  def processFile(file: File, isIncremental: Boolean, checker: (String, String), prog: (String, String)): Double = {
    val lines = io.Source.fromFile(file).getLines()

    val head = lines.next()

    var rawData = List[(Int, Double)]()
    for (line <- lines) {
      val ar = line.split(" ")

      val height = ar(1).toInt
      val timeMillis = ar(2).toDouble
      rawData = rawData :+ (height, timeMillis)
    }

    val maxHeight = rawData.map(_._1).max

    var dataPoints = List[DataPoint]()
    for ((height, timeMillis) <- rawData) {
      val data =
        if (isIncremental)
          IncrementalDataPoint(maxHeight, height, timeMillis)
        else
          NonincrementalDataPoint(height, timeMillis)

      dataPoints = dataPoints :+ data
    }

    val avg = dataPoints.map(_.perfomance).sum / dataPoints.size
    avg
  }

  trait DataPoint {
    val perfomance: Double
  }

  case class IncrementalDataPoint(maxHeight: Int, currentHeight: Int, timeMillis: Double) extends DataPoint {
    val maxSize = Math.pow(2, maxHeight).toInt - 1
    val perfomance = maxSize.toDouble / timeMillis
  }

  case class NonincrementalDataPoint(currentHeight: Int, timeMillis: Double) extends DataPoint {
    val treeSize = Math.pow(2, currentHeight).toInt - 1
    val perfomance = treeSize.toDouble / timeMillis
  }
}
