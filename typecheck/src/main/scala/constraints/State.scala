package constraints

import incremental.Util

import scala.collection.mutable

case class State[G <: GenBase](gen: G) {
  val stats: mutable.Map[String, Double] = mutable.Map().withDefaultValue(0.0)
  def +=(kv: (String, Double)) = stats += kv._1 -> (stats(kv._1) + kv._2)

  def printStatistics(): Unit = {
    val maxwidth = stats.keys.foldLeft(0)((max, k) => Math.max(max, k.length))
    for (k <- stats.keys) {
      val restwidth = maxwidth - k.length
      val space = " " * restwidth
      if (k.toLowerCase.endsWith("count"))
        Util.log(f"$k$space = ${stats(k).toInt}")
      else
        Util.log(f"$k$space = ${stats(k)}%.3fms")
    }
  }
}
