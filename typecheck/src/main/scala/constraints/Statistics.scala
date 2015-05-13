package constraints

import scala.language.implicitConversions
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

import incremental.Util

import scala.util.Sorting


object StatKeys extends Enumeration {
  type StatKeys = Value
  val TypeCheck, SolveConstraint, MergeReqs, MergeSolution, Finalize = Value

  implicit def valueToInt(v: Value): Int = v.id
}

class Statistics {
  final type Timestamp = Long
  final type Interval = (Timestamp, Timestamp)

    /**
     *  Order of intervals along the time axis, groups together overlapping intervals,
     *  where longer intervals precede shorter ones.
     */
  object IntervalOrdering extends Ordering[Interval] {
    def compare(x: Interval, y: Interval) = {
      val ((xstart, xend),(ystart, yend)) = (x, y)
      if (ystart == xstart && xend == yend)
        0
      else if (ystart <= xstart && xend <= yend)
        -1
      else if (xstart <= ystart && yend <= xend)
        1
      else if (xstart <= yend)
        -1
      else
        1
    }
  }


  private val q: Array[ConcurrentLinkedQueue[Interval]] = Array.fill(StatKeys.maxId)(new ConcurrentLinkedQueue[Interval]())
  private var _constraintCount = new AtomicInteger(0)
  @inline
  final def addToConstraintCount(i: Int): Unit = _constraintCount.addAndGet(i)

  final def apply[T](evt: StatKeys.Value)(thunk: => T): T = {
    val start = System.nanoTime()
    val res = thunk
    val end = System.nanoTime()
    q(evt.id).add((start, end))
    res
  }

  def constraintCount: Int = _constraintCount.get()

  def resetStats() = {
    _constraintCount = new AtomicInteger(0)
    var i = 0
    while (i < q.length) {
      q.update(i, new ConcurrentLinkedQueue[Interval]())
      i += 1
    }
  }

  def print(): Unit = {
    import StatKeys._
    val times = postProcess()

    val values = Seq("constraintCount"-> _constraintCount.get().toString,
    "constraintSolveTime"-> times(SolveConstraint).toString,
    "mergeSolutionTime"-> times(MergeSolution).toString,
    "mergeReqsTime"-> times(MergeReqs).toString,
    "finalizeTime"->times(Finalize).toString,
    "typecheckTime"->times(TypeCheck).toString)

    val maxwidth = values.foldLeft(0)((max, kv) => Math.max(max, kv._1.length))


    for (kv <- values) {
      val k = kv._1
      val v = kv._2
      val restwidth = maxwidth - k.length
      val space = " " * restwidth
      if (v.toDouble > 0) {
        if (kv._1.toLowerCase.endsWith("count"))
          Util.log(f"$k$space = ${v}")
        else
          Util.log(f"$k$space = ${v.toDouble}%.3fms")
      }
    }
  }



  final def postProcess(): Array[Double] = {
    import scala.collection.JavaConverters._

    val res = q map { queue =>
      val evts = queue.asScala.toArray
      Sorting.quickSort(evts)(IntervalOrdering)
      val time = calculateTime(evts)
      time/1000000d
    }

    res
  }

  private final def calculateTime(a: Array[Interval]): Timestamp = {
    //precondition: a is sorted according to the order defined in class IntervalOrdering
    @inline
    def disjoint(start: Timestamp, end: Timestamp, start1: Timestamp, end1: Timestamp) = start < end1 || start1 < end
    @inline
    def merge(start: Timestamp, end: Timestamp, start1: Timestamp, end1: Timestamp) = (start.min(start1), end.max(end1))
    val (sum, s,e) = a.foldLeft((0l, 0l, 0l)) {
      case ((sum, s,e), (s1, e1)) =>
        if (disjoint(s,e,s1,e1)) {
          (sum + (e - s), s1, e1)
        }
        else {
          val (s2, e2) = merge(s, e, s1, e1)
          (sum, s2, e2)
        }
    }
    sum + (e-s)
  }
}

