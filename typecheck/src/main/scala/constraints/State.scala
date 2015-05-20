package constraints

import constraints.Statistics.Statistics
import incremental.Util


case class State[G <: GenBase](gen: G) {
  private val _stats = Array.fill(Statistics.maxId)(0.0)

  def stats(k: Statistics) = _stats(k.id)

  def +=(kv: (Statistics, Double)) = {
    val i = kv._1.id
    val oldV = _stats(i)
    val newV = oldV + kv._2
    _stats(i) = newV
  }

  def printStatistics(): Unit = {
    val maxwidth = Statistics.values.foldLeft(0)((max, k) => Math.max(max, k.toString.length))
    for (k <- Statistics.values) {
      val restwidth = maxwidth - k.toString.length
      val space = " " * restwidth
      if (_stats(k.id) > 0) {
        if (k.toString.toLowerCase.endsWith("count"))
          Util.log(f"$k$space = ${_stats(k.id).toInt}")
        else
          Util.log(f"$k$space = ${_stats(k.id)}%.3fms")
      }
    }
  }

  def resetStats() = for (i <- 0 until _stats.length) _stats(i) = 0.0

}
