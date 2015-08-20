package benchmark

import java.io.{PrintWriter, File}
import java.util.Locale
import java.util.regex.Pattern

import scopt.OptionParser

import scala.collection.immutable.{ListMap, TreeMap}

object ProcessData extends App {


  case class Config(
    dir: File,
    isIncrementalData: Boolean,
    progs: Map[String, String], // file name fragment -> latex representation
    checkers: Map[String, String], // file name fragment -> latex representation
    baselineChecker: String,
    outfile: File
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

    opt[String]("baseline") required() action { (baseline, config) =>
      config.copy(baselineChecker = baseline)
    }

    opt[File]('o', "outfile") required() action { (f, config) =>
      config.copy(outfile = f)
    }

    arg[File]("<directory>") action { (dir, config) =>
      config.copy(dir = dir)
    } validate { f =>
      if (f.exists()) success else failure(s"directory not found $f")
    }
  }

  optionParser.parse(args, Config(null, false, pcfProgs, pcfCheckers, "", null)) match {
    case None =>
    case Some(config) => run(config)
  }

  def run(config: Config): Unit = {

    val files = config.dir.listFiles()

    val rows = config.progs.size + 1
    val cols = config.checkers.size + 1
    val rawtable = Array.ofDim[Option[Double]](rows, cols)
    val table = Array.ofDim[String](rows + 1, cols) // list of rows

    for (checker <- config.checkers;
         prog <- config.progs) {

      val fileRegex = Pattern.quote(s"${prog._1}.${checker._1}.Test-") + "\\d*" + Pattern.quote(".dsv")
      val mfile = files.find(f => f.getName.matches(fileRegex))
      val rowIndex = config.progs.toList.indexOf(prog) + 1
      val colIndex = config.checkers.toList.indexOf(checker) + 1

      mfile match {
        case None =>
          rawtable(rowIndex)(colIndex) = None
        case Some(file) =>
          val avg = processFile(file, config.isIncrementalData, checker, prog)
          rawtable(rowIndex)(colIndex) = Some(avg)
      }
    }

    table(0)(0) = "Tree"
    for (checker <- config.checkers) {
      val colIndex = config.checkers.toList.indexOf(checker) + 1
      table(0)(colIndex) = checker._2
    }
    for (prog <- config.progs) {
      val rowIndex = config.progs.toList.indexOf(prog) + 1
      table(rowIndex)(0) = prog._2
    }
    val baselineCol = config.checkers.toList.unzip._1.indexOf(config.baselineChecker) + 1

    for (row <- 1 until rows; col <- 1 until cols) {
      if (col == baselineCol) {
        table(row)(col) = math(rawtable(row)(col).get)
      }
      else {
        val cell = rawtable(row)(col) match {
          case None => "\\hfake{600.00 (0.00)}{n/a}"
          case Some(avg) =>
            val speedup = avg / rawtable(row)(baselineCol).get
            math(avg) + " (" + math(speedup) + ")"
        }
        table(row)(col) = cell
      }
    }

    val last = table.length - 1
    table(last)(0) = "{\\scriptsize overall performance}"
    for (col <- 1 until cols) {
      var numVals = 0
      var sumPerf = 0.0
      var sumSpeedup = 0.0
      for (row <- 1 until rows) {
        rawtable(row)(col) match {
          case None =>
          case Some(avg) =>
            numVals += 1
            sumPerf += avg
            val speedup = avg / rawtable(row)(baselineCol).get
            sumSpeedup += speedup
        }
       }

      if (numVals != 0 && col != baselineCol) {
        val avgPerformance = sumPerf / numVals
        val avgSpeedup = sumSpeedup / numVals
        val cell = math(avgPerformance) + " (" + math(avgSpeedup) + ")"
        table(last)(col) = cell
      }
      else if (numVals != 0) {
        val avgPerformance = sumPerf / numVals
        val cell = math(avgPerformance)
        table(last)(col) = cell
      }
      else
        table(last)(col) = "n/a"
    }

    new PrintWriter(config.outfile) { write(prettyPrintTable(table)); close }
  }

  def rowString(strings: Array[String]) = strings.mkString("", " & ", "\\\\")

  def prettyPrintTable(table: Array[Array[String]]) = {
    val buffer = StringBuilder.newBuilder
    val cols = (1 to table(0).length).map(_ => "r").mkString("@{\\hskip1em}", "@{\\hskip1em}", "")
    buffer ++=  s"\\begin{tabular}[t]{l$cols}\n"
    buffer ++= "\\toprule\n"
    buffer ++= rowString(table(0)) += '\n'
    buffer ++= "\\midrule\n"
    for (row <- 1 until table.length) {
      buffer ++= rowString(table(row))
      if (row % 3 == 0)
        buffer ++= "\\midrule"
      buffer += '\n'
    }
    buffer ++= "\\bottomrule\n"
    buffer ++= "\\end{tabular}"
    buffer.toString()
  }


  def math(s: Double) = "$" + String.format(Locale.US, "%.2f", Double.box(s)) + "$"

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
