package incremental.pcf.concurrent

import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.util.concurrent.{Executors, ConcurrentHashMap}

import constraints.CVar
import constraints.equality.Type
import incremental.Node._
import incremental.Node_
import incremental.pcf._

import util.Join
import util.Join.Join

import scala.collection.JavaConversions
import scala.collection.mutable.ListBuffer

/**
 * Created by oliver on 24.06.15.
 */
object LightweightChecker {
  type TError = String
  type Reqs = Map[Symbol, Type]
  type StepResult = (Type, Reqs, Seq[EqConstraint])
  type Result = (Type, Reqs, CS)

  abstract class Checker {
    final def typecheckImpl(e: Node): Either[Type, TError] = {
      val root = e.withType[Result]
      var i = 0
      while (i < ids.length) {
        ids(i) = 0
        i += 1
      }
      doCheck(root)

      /*root.visitUninitialized { e =>
        typecheckRec(e)
        true
      }*/

      val (t_, reqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = subst(t_, sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }

    def doCheck(root: Node_[Result]): Unit

    final def typecheckRec(e: Node_[Result])(thread: Int): Unit = {
      val res@(t, reqs, cons) = typecheckStep(e)(thread)
      val subcs = e.kids.seq.foldLeft(CS())((cs, res) => cs mergeSubsystem res.typ._3)
      val cs = subcs addNewConstraints cons
      val reqs2 = reqs.mapValues(subst(_, cs.substitution))
      e.typ = (cs applyPartialSolution t, reqs2, cs.propagate)
    }


    final def typecheckStep(e: Node_[Result])(thread: Int): StepResult = e.kind match {
      case Num => (TNum, Map(), Seq())
      case op if op == Add || op == Mul =>
        val (t1, reqs1, _) = e.kids(0).typ
        val (t2, reqs2, _) = e.kids(1).typ

        val lcons = EqConstraint(TNum, t1)
        val rcons = EqConstraint(TNum, t2)
        val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

        (TNum, mreqs, mcons :+ lcons :+ rcons)
      case Var =>
        val x = e.lits(0).asInstanceOf[Symbol]
        val X = freshUVar(thread)
        (X, Map(x -> X), Seq())
      case App =>
        val (t1, reqs1, _) = e.kids(0).typ
        val (t2, reqs2, _) = e.kids(1).typ

        val X = freshUVar(thread)
        val fcons = EqConstraint(TFun(t2, X), t1)
        val (mcons, mreqs) = mergeReqMaps(reqs1, reqs2)

        (X, mreqs, mcons :+ fcons)
      case Abs if (e.lits(0).isInstanceOf[Symbol]) =>
        val x = e.lits(0).asInstanceOf[Symbol]
        val (t, reqs, _) = e.kids(0).typ

        reqs.get(x) match {
          case None =>
            val X = if (e.lits.size == 2) e.lits(1).asInstanceOf[Type] else freshUVar(thread)
            (TFun(X, t), reqs, Seq())
          case Some(treq) =>
            val otherReqs = reqs - x
            if (e.lits.size == 2) {
              (TFun(treq, t), otherReqs, Seq(EqConstraint(e.lits(1).asInstanceOf[Type], treq)))
            }
            else
              (TFun(treq, t), otherReqs, Seq())
        }
      case Abs if (e.lits(0).isInstanceOf[Seq[_]]) =>
        val xs = e.lits(0).asInstanceOf[Seq[Symbol]]
        val (t, reqs, _) = e.kids(0).typ

        val Xs = xs map (_ => freshUVar(thread))

        var restReqs = reqs
        var tfun = t
        for (i <- xs.size - 1 to 0 by -1) {
          val x = xs(i)
          restReqs.get(x) match {
            case None =>
              val X = freshUVar(thread)
              tfun = TFun(X, tfun)
            case Some(treq) =>
              restReqs = restReqs - x
              tfun = TFun(treq, tfun)
          }
        }

        (tfun, restReqs, Seq())
    }
  }

  case class CS(substitution: Map[Symbol, Type], notyet: Seq[EqConstraint], never: Seq[EqConstraint]) {
    def applyPartialSolution(t: Type) = subst(t, substitution)

    def addNewConstraints(cons: Seq[EqConstraint]) = cons.foldLeft(this)((cs, c) => c.solve(cs))

    def mergeSubsystem(other: CS) = CS(Map(), notyet ++ other.notyet, never ++ other.never)

    def notyet(c: EqConstraint): CS = CS(substitution, notyet :+ c, never)
    def never(c: EqConstraint): CS  = CS(substitution, notyet, never :+ c)

    def solved(s: Map[Symbol, Type]) = {
      var current = CS(substitution mapValues (x => subst(x, s)), notyet, never)
      for ((x, t2) <- s) {
        current.substitution.get(x) match {
          case None => current = CS(current.substitution + (x -> subst(t2, current.substitution)), current.notyet, current.never)
          case Some(t1) => current = unify(t1, t2, current)
        }
      }
      current
    }

    def unsolved = notyet ++ never
    def isSolved = notyet.isEmpty && never.isEmpty
    def solvable = never.nonEmpty
    def isSolvable: Boolean = never.isEmpty

    def tryFinalize = trySolve(true)

    def propagate: CS = CS(Map(), notyet.map(subst(_, substitution)), never.map(subst(_, substitution)))

    private def trySolve(finalize: Boolean): CS = {
      var current = this
      var stepsWithoutChange = 0
      while (!current.notyet.isEmpty) {
        val next = current.notyet.head
        val rest = current.notyet.tail
        current = CS(current.substitution, rest, current.never)
        current = next.solve(current)

        if (current.notyet.size == rest.size + 1) {
          stepsWithoutChange += 1
          if (stepsWithoutChange > rest.size + 1)
            return current
        }
        else
          stepsWithoutChange = 0
      }
      current
    }
  }
  object CS {
    def apply(): CS = new CS(Map(), Seq(), Seq())
  }

  case class EqConstraint(expected: Type, actual: Type) {
    def solve(cs: CS) = unify(expected, actual, cs)
  }

  def unify(t1: Type, t2: Type, cs: CS): CS = (t1, t2) match {
    case (UVar(x), other) if t1 == t2 => cs
    case (UVar(cv@CVar(x)), other) => cs.substitution.get(x) match {
      case Some(t) => unify(t, other, cs)
      case None =>
        val t = subst(other, cs.substitution)
        if (t1 == t)
          cs
        else if (t.occurs(cv))
          cs.never(EqConstraint(t1, t))
        else
          cs.solved(Map(x -> t))
    }
    case (_, UVar(_)) => unify(t2, t1, cs)
    case (TNum, TNum) => cs
    case (TFun(t1_, t2_), TFun(t1__, t2__)) => unify(t2_, t2__, unify(t1_, t1__, cs))
    case _ => cs.never(EqConstraint(t1, t2))
  }

  def subst(t: Type, sigma: Map[Symbol, Type]): Type = t match {
    case UVar(CVar(x)) => sigma.getOrElse(x, t)
    case TNum => TNum
    case TFun(t1, t2) =>
      var args = List(subst(t1, sigma))
      var res = t2
      while (res.isInstanceOf[TFun]) {
        val resfun = res.asInstanceOf[TFun]
        args = subst(resfun.t1, sigma) :: args
        res = resfun.t2
      }
      res = subst(res, sigma)
      for (a <- args)
        res = TFun(a, res)
      res
  }

  def subst(eq: EqConstraint, sigma: Map[Symbol, Type]): EqConstraint = EqConstraint(subst(eq.actual, sigma), subst(eq.expected, sigma))

  def mergeReqMaps(req: Reqs, reqs: Reqs*): (Seq[EqConstraint], Reqs) = mergeReqMaps(req +: reqs)

  def mergeReqMaps(reqs: Seq[Reqs]): (Seq[EqConstraint], Reqs) =
      reqs.foldLeft[(Seq[EqConstraint], Reqs)](init)(_mergeReqMaps)

  private val init: (Seq[EqConstraint], Reqs) = (Seq(), Map())

  private def _mergeReqMaps(was: (Seq[EqConstraint], Reqs), newReqs: Reqs) = {
    val wasReqs = was._2
    var mcons = was._1
    var mreqs = wasReqs
    for ((x, r2) <- newReqs)
      wasReqs.get(x) match {
        case None => mreqs += x -> r2
        case Some(r1) =>
          mcons = EqConstraint(r1, r2) +: mcons
      }
    (mcons, mreqs)
  }

  def freshUVar(index: Int): UVar = {
    val next = ids(index)
    ids(index) += 1
    UVar(CVar(Symbol("x$" + index + "$" + next)))
  }

  val ids = Array.fill(Runtime.getRuntime.availableProcessors())(0)
}

class SequentialChecker extends LightweightChecker.Checker {
  import LightweightChecker._
  def doCheck(root: Node_[Result]) = root.visitUninitialized { e =>
    typecheckRec(e)(0)
    true
  }
}

object WorkStealingChecker {
  val stats = Array.fill(Runtime.getRuntime.availableProcessors())(0l)

  final class ThreadPool {
    private var _live = false
    private val buffer = collection.mutable.ListBuffer[Runnable]()
    private val pool = Executors.newWorkStealingPool(Runtime.getRuntime.availableProcessors())
    //private val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())


    def start(): Unit = {
      _live = true
      buffer.foreach(pool.execute(_))
      buffer.clear()
    }

    def shutdown() = pool.shutdownNow()

    def stop(): Unit = {
      _live = false
//      var i = 0
//      while (i < stats.length) {
//        println(s"Thread $i: ${stats(i)} ms")
//        stats(i) = 0l
//        i += 1
//      }
    }

    @inline
    def submit(thunk: Runnable) = {
      //queue.add(thunk)
      if (!_live) buffer += thunk
      else pool.execute(thunk)
    }
  }


  val pool = new ThreadPool
}


class WorkStealingChecker(val clusterParam: Int = 2) extends LightweightChecker.Checker {
  import LightweightChecker._

  @inline
  private final def threadId = Thread.currentThread().getId.toInt % WorkStealingChecker.stats.length

  private final def processNode(e: Node_[Result]): Unit = {
    e.visitInvalid { e =>
      typecheckRec(e)(threadId)
      true
    }
  }

  private def isJoinNode(e: Node_[Result]): Boolean = (e.height - clusterParam) % (clusterParam + 1) == 0

  val stats = Array.fill(Runtime.getRuntime.availableProcessors())(new ListBuffer[String])
  val colors = Array("red", "green", "blue", "orange")

  final def work(e: Node_[Result], id: Int, parent: Join): Unit = {
    work(new Runnable { def run() = {
    //  val start = System.currentTimeMillis()
      stats(threadId) += "%d -> %d".format(id, parent.id)
      stats(threadId) += "%d [label=%d,color=%s]".format(id, Thread.currentThread().getId, colors(threadId))
      processNode(e)
      parent.leave()
     // val end = System.currentTimeMillis()
     // WorkStealingChecker.stats(threadId) += (end - start)
    } })
  }
  final def work(thunk: Runnable): Unit = { WorkStealingChecker.pool.submit(thunk) }
  def prepareSchedule(root: Node_[Result]): Join = {
    var leafId = 32768
    var innerJoins = 0
    val typeCheckDone: Join = Join(0)
    def recurse(e: Node_[Result], parentJoin: Join): Unit = {
      if (e.height <= clusterParam) {
        parentJoin.join()
        work(e, leafId, parentJoin)
        util.Join.count += 1
        leafId += 1
      }
      else {
        if (isJoinNode(e)) {
          val nodeJoin = Join(0)
          parentJoin.join()
          nodeJoin andThen work(e, nodeJoin.id, parentJoin)
          innerJoins += 1
          e.kids.seq.foreach { k => recurse(k, nodeJoin) }
        }
        else e.kids.seq.foreach { k => recurse(k, parentJoin) }
      }
    }

    if (!isJoinNode(root)) {
      val joinRoot = Join(0)
      typeCheckDone.join()
      joinRoot andThen work(root, joinRoot.id, typeCheckDone)
      root.kids.seq.foreach { k => recurse(k, joinRoot) }
    }
    else recurse(root, typeCheckDone)
    println(s"produced $innerJoins inner joins")
    typeCheckDone
  }


  def doCheck(root: Node_[Result]) = {
    util.Join.count = -1
    println(s"root has height ${root.height} and size ${root.size}")
    val join = prepareSchedule(root)
    var complete = false
    join andThen {
      this.synchronized {
        complete = true
        this.notify()
      }
    }
    WorkStealingChecker.pool.start()
    this.synchronized {
      while (!complete)
        this.wait()
    }
    WorkStealingChecker.pool.stop()
    println(s"PRODUCED ${util.Join.count} joins")
    writeFile()
  }

  def writeFile(): Unit = {
    val writer = new PrintWriter(new File("graph-" + LocalDateTime.now() + ".dot"))
    try {
      writer.println("digraph tree {")
      for(items <- stats; item <- items)
        writer.println(item)
      writer.println("}")
    }
    finally writer.close()
  }
}
