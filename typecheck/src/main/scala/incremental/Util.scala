package incremental

import constraints.State
import constraints.Statistics.Statistics
import util.TreeSeq

import scala.collection.mutable

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

  def timed[A](statsKey: (State[_], Statistics))(f: => A): A = {
    val start = System.nanoTime()
    val a = f
    val end = System.nanoTime()
    val time = (end-start)/1000000.0
    statsKey._1 += statsKey._2 -> time
    a
  }

  def log(s: String): Unit = {
    if (LOGGING)
      println(s)
  }


  def loop[Obj, In, Product](f: (Obj, In) => (Obj, TreeSeq[Product]))(init: Obj, ins: Iterable[In]): (Obj, TreeSeq[Product]) = {
    var o = init
    var prods = TreeSeq[Product]()
    for (in <- ins) {
      val (newo, newprods) = f(o, in)
      o = newo
      prods = prods ++ newprods
    }
    (o, prods)
  }
}

class MyBuilder[K,V] extends mutable.Builder[((K, V), V), Map[K, V]] {
  var res = Map[K,V]()
  def +=(el: ((K, V), V)) = {res = res + (el._1._1 -> el._2); this}
  def clear() = res = Map()
  def result() = res
}

