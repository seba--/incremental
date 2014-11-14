package incremental

/**
 * Created by seba on 06/11/14.
 */
object Util {
  val LOGGING = true
  val LOG_TABLE_OPS = false

  def logTime[A](desc: String)(f: => A): A = {
    val (a, time) = timed(f)
    if (LOGGING)
      println(s"Time to $desc is ${time}ms")
    a
  }

  def timed[A](f: => A): (A, Double) = {
    val start = System.nanoTime()
    val a = f
    val end = System.nanoTime()
    val time = (end-start)/1000000.0
    (a, time)
  }

  def log(s: String): Unit = {
    if (LOGGING)
      println(s)
  }
}
