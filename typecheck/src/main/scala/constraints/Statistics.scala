package constraints

import java.util.concurrent.atomic.{AtomicLong, AtomicInteger}

import incremental.Util

/**
 * Created by seba on 13/11/14.
 */

trait Statistics {
  def constraintCount: Int
  def addToConstraintCount(i: Int): Unit
  def setConstraintCount(i: Int): Unit

  def constraintSolveTime: Double
  def addToConstraintSolveTime(i: Double): Unit
  def setConstraintSolveTime(i: Double): Unit
  def constraintSolveTimed[A](f: => A): A = {
    val (res, time) = Util.timed(f)
    addToConstraintSolveTime(time)
    res
  }

  def mergeSolutionTime: Double
  def addToMergeSolutionTime(i: Double): Unit
  def setMergeSolutionTime(i: Double): Unit
  def mergeSolutionTimed[A](f: => A): A = {
    val (res, time) = Util.timed(f)
    addToMergeSolutionTime(time)
    res
  }

  def mergeReqsTime: Double
  def addToMergeReqsTime(i: Double): Unit
  def setMergeReqsTime(i: Double): Unit
  def mergeReqsTimed[A](f: => A): A = {
    val (res, time) = Util.timed(f)
    addToMergeReqsTime(time)
    res
  }

  def finalizeTime: Double
  def addToFinalizeTime(i: Double): Unit
  def setFinalizeTime(i: Double): Unit
  def finalizeTimed[A](f: => A): A = {
    val (res, time) = Util.timed(f)
    addToFinalizeTime(time)
    res
  }

  def typecheckTime:Double
  def addToTypecheckTime(i: Double): Unit
  def setTypecheckTime(i: Double): Unit
  def typecheckTimed[A](f: => A): A = {
    val (res, time) = Util.timed(f)
    addToTypecheckTime(time)
    res
  }

  def resetStats() = {
    setConstraintCount(0)
    setConstraintSolveTime(0)
    setMergeSolutionTime(0)
    setMergeReqsTime(0)
    setFinalizeTime(0)
    setTypecheckTime(0)
  }

  def stringValues = Seq(
    "constraintCount"->constraintCount.toString,
    "constraintSolveTime"->constraintSolveTime.toString,
    "mergeSolutionTime"->mergeSolutionTime.toString,
    "mergeReqsTime"->mergeReqsTime.toString,
    "finalizeTime"->finalizeTime.toString,
    "typecheckTime"->typecheckTime.toString)

  def print(): Unit = {
    val values = stringValues
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
}

class LocalStatistics extends Statistics {
  private var _constraintCount: Int = 0
  def constraintCount = _constraintCount
  def addToConstraintCount(i: Int) = _constraintCount += i
  def setConstraintCount(i: Int)= _constraintCount = i

  private var _constraintSolveTime: Double = 0
  def constraintSolveTime = _constraintSolveTime
  def addToConstraintSolveTime(i: Double) = _constraintSolveTime += i
  def setConstraintSolveTime(i: Double) = _constraintSolveTime = i

  private var _mergeSolutionTime: Double = 0
  def mergeSolutionTime = _mergeSolutionTime
  def addToMergeSolutionTime(i: Double) = _mergeSolutionTime += i
  def setMergeSolutionTime(i: Double) = _mergeSolutionTime = i

  private var _mergeReqsTime: Double = 0
  def mergeReqsTime = _mergeReqsTime
  def addToMergeReqsTime(i: Double) = _mergeReqsTime += i
  def setMergeReqsTime(i: Double) = _mergeReqsTime = i

  private var _finalizeTime: Double = 0
  def finalizeTime = _finalizeTime
  def addToFinalizeTime(i: Double) = _finalizeTime += i
  def setFinalizeTime(i: Double) = _finalizeTime = i

  private var _typecheckTime: Double = 0
  def typecheckTime = _typecheckTime
  def addToTypecheckTime(i: Double) = _typecheckTime += i
  def setTypecheckTime(i: Double) = _typecheckTime = i
}

class ThreadedStatistics extends Statistics {
  private val _constraintCount = new AtomicInt(new AtomicInteger(0))
  def constraintCount = _constraintCount.get
  def addToConstraintCount(i: Int) = _constraintCount += i
  def setConstraintCount(i: Int)= _constraintCount.set(i)

  private val _constraintSolveTime = new AtomicDouble(new AtomicLong(0l))
  def constraintSolveTime = _constraintSolveTime.get
  def addToConstraintSolveTime(i: Double) = _constraintSolveTime += i
  def setConstraintSolveTime(i: Double) = _constraintSolveTime.set(i)

  private val _mergeSolutionTime = new AtomicDouble(new AtomicLong(0l))
  def mergeSolutionTime = _mergeSolutionTime.get
  def addToMergeSolutionTime(i: Double) = _mergeSolutionTime += i
  def setMergeSolutionTime(i: Double) = _mergeSolutionTime.set(i)

  private val _mergeReqsTime = new AtomicDouble(new AtomicLong(0l))
  def mergeReqsTime = _mergeReqsTime.get
  def addToMergeReqsTime(i: Double) = _mergeReqsTime += i
  def setMergeReqsTime(i: Double) = _mergeReqsTime.set(i)

  private val _finalizeTime = new AtomicDouble(new AtomicLong(0l))
  def finalizeTime = _finalizeTime.get
  def addToFinalizeTime(i: Double) = _finalizeTime += i
  def setFinalizeTime(i: Double) = _finalizeTime.set(i)

  private val _typecheckTime = new AtomicDouble(new AtomicLong(0l))
  def typecheckTime = _typecheckTime.get
  def addToTypecheckTime(i: Double) = _typecheckTime += i
  def setTypecheckTime(i: Double) = _typecheckTime.set(i)

}

class AtomicInt(val ai: AtomicInteger) extends AnyVal {
  def +=(d: Int): Int = ai.addAndGet(d)

  def get: Int = ai.get()
  
  def set(i: Int) = ai.set(i)
}

class AtomicDouble(val al: AtomicLong) extends AnyVal {
  def +=(d: Double): Double = {
    while (true) {
      val oldBits = al.get()
      val oldV = java.lang.Double.longBitsToDouble(oldBits)
      val newV = oldV + d
      val newBits = java.lang.Double.doubleToLongBits(newV)
      if (al.compareAndSet(oldBits, newBits))
        return newV
    }
    throw new Error() // unreachable
  }

  def get = java.lang.Double.longBitsToDouble(al.get())
  
  def set(d: Double) = al.set(java.lang.Double.doubleToLongBits(d))
}

