package incremental

import constraints.State
import constraints.Statistics.Statistics

import scala.collection.immutable.HashSet
import scala.collection.{GenTraversableOnce, mutable}
import scala.util.hashing.MurmurHash3

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


  def loop[Obj, In, Product](f: (Obj, In) => (Obj, Seq[Product]))(init: Obj, ins: Iterable[In]): (Obj, Seq[Product]) = {
    var o = init
    var prods = Seq[Product]()
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

object IncHashedSet {
  def empty[T]: IncHashedSet[T] = {
    val set = HashSet.empty[T]
    new IncHashedSet[T](set, new IncrementalUnorderedHash())
  }
  def apply[T]() = empty[T]
  def apply[T](t: T) = empty[T] + t
  def fromCol[T](ts: Iterable[T]) = ts.foldLeft(empty[T])(_+_)

  class IncrementalUnorderedHash(val a: Int = 0, val b: Int = 0, val n: Int = 0) {
    def +(x: Any): IncrementalUnorderedHash = {
      val h = x.##
      val newa = a + h
      val newb = b ^ h
      val newn = n + 1
      new IncrementalUnorderedHash(newa, newb, newn)
    }

    def ++(other: IncrementalUnorderedHash) = {
      val newa = a + other.a
      val newb = b ^ other.b
      val newn = n + other.n
      new IncrementalUnorderedHash(newa, newb, newn)
    }

    def -(x: Any): IncrementalUnorderedHash = {
      val h = x.##
      val newa = a - h
      val newb = b ^ h
      val newn = n - 1
      new IncrementalUnorderedHash(newa, newb, newn)
    }

    override def hashCode(): Int = {
      var h = MurmurHash3.setSeed
      h = MurmurHash3.mix(h, a)
      h = MurmurHash3.mixLast(h, b)
      MurmurHash3.finalizeHash(h, n)
    }
  }
}

class IncHashedSet[T](val set: Set[T], private val hash: IncHashedSet.IncrementalUnorderedHash) {
  def +(t: T): IncHashedSet[T] = new IncHashedSet(set + t, hash + t)
  def ++(other: IncHashedSet[T]): IncHashedSet[T] = new IncHashedSet(set union (other.set), hash ++ other.hash)
  def -(t: T): IncHashedSet[T] = new IncHashedSet(set - t, hash - t)
  def isEmpty: Boolean = set.isEmpty
  def contains(t: T): Boolean = set.contains(t)
  def foreach(f: T => Unit): Unit = set.foreach(f)
  def mkString(sep: String): String = set.mkString(sep)

  def map[B](f: T => B): IncHashedSet[B] = {
    val builder = Set.newBuilder[B]
    var hash = new IncHashedSet.IncrementalUnorderedHash()
    for (t <- set) {
      val b = f(t)
      builder += b
      hash += b
    }
    new IncHashedSet(builder.result(), hash)
  }

  def flatMap(f: T => GenTraversableOnce[T]): IncHashedSet[T] = {
    val builder = Set.newBuilder[T]
    var hash = new IncHashedSet.IncrementalUnorderedHash()
    for (t <- set;
         newt <- f(t)) {
      builder += newt
      hash += newt
    }
    new IncHashedSet(builder.result(), hash)
  }

  override def hashCode(): Int = hash.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: IncHashedSet[_] => set == that.set
    case _ => false
  }
}
