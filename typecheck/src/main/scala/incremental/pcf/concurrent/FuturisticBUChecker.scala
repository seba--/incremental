package incremental.pcf.concurrent

import java.util.concurrent.{Executors, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicInteger

import constraints.StatKeys._
import constraints.equality.{ConstraintSystem, ConstraintSystemFactory}
import incremental.Node.Node
import incremental.Node_

import incremental.pcf._
import util.Join

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.io.StdIn
import scala.language.postfixOps

object FuturisticBUChecker {
  //implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors()))
 // implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
}

/**
 * Created by seba on 13/11/14.
 */
abstract class FuturisticBUChecker[CS <: ConstraintSystem[CS]] extends BUChecker[CS](true) {

  val clusterParam = 31
  def bottomUpFuture(e: Node_[Result]): (Future[Any], Promise[Unit]) = {
    val trigger: Promise[Unit] = Promise()
    val fut = trigger.future
    def recurse(e: Node_[Result]): (Seq[Future[Any]], Int) = {
      if (e.size == clusterParam) {
        val f = fut map { _ =>
          e.visitInvalid { e =>
            typecheckRec(e)
            true
          }
        }
        (Seq(f), e.size)
      }
      else {
        val (fs, nodecounts) = (e.kids.seq.map { k => recurse(k) }).unzip
        val nodecount = nodecounts.sum
        val remaining = e.size - nodecount

        if (remaining >= clusterParam) { //TODO == ?
          val join = Future.fold(fs.flatten)(()) { (x,y) => () }
          val future = join.map { _ =>
              e.visitInvalid { e =>
                typecheckRec(e)
                true
              }
            }
          (Seq(future), e.size)
        }
        else
          (fs.flatten, nodecount)
      }
    }

    val (fs, nodecount) = recurse(e)
    val res = Future.sequence(fs)

    if (e.size != nodecount)  {
      val res2 = res map { _ =>
        e.visitInvalid { e =>
          typecheckRec(e)
          true
        }
      }
      (res2, trigger)
    }
    else (res, trigger)
  }




  override def typecheckImpl(e: Node): Either[T, TError] = {
    val root = e.withType[Result]
    val (fut, trigger) = bottomUpFuture(root)

    localState.stats(TypeCheck) {
      trigger success ()
      Await.result(fut, 1 minute)

      val (t_, reqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
  }
}

case class FuturisticBUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new FuturisticBUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}

abstract class SingleFutureBUChecker[CS <: ConstraintSystem[CS]] extends FuturisticBUChecker[CS] {
  override def bottomUpFuture(e: Node_[Result]): (Future[Any], Promise[Unit]) = {
    val trigger: Promise[Unit] = Promise()
    val fut = trigger.future
    val res = fut map { _ =>
      e.visitUninitialized {
        e =>
          typecheckRec(e)
          true
      }
    }
    (res, trigger)
  }
}

case class SingleFutureBUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new SingleFutureBUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}

abstract class FuturisticHeightBUChecker[CS <: ConstraintSystem[CS]] extends FuturisticBUChecker[CS] {
  override val clusterParam = 4
  override def bottomUpFuture(e: Node_[Result]): (Future[Any], Promise[Unit]) = {
    val trigger: Promise[Unit] = Promise()
    val fut = trigger.future
    def recurse(e: Node_[Result]): (Seq[Future[Any]], Int) = {
      if (e.height == clusterParam) {
        val f = fut map { _ =>
          e.visitInvalid { e =>
            typecheckRec(e)
            true
          }
        }
        (Seq(f), 1)
      }
      else {
        val (fs, hops) = (e.kids.seq.map { k => recurse(k) }).unzip
        val max = hops.foldLeft(0) { case (i,j) => i.max(j) }

        if ((max % clusterParam) == 0) {
          val join = Future.fold(fs.flatten)(()) { case x => () }
          val future = join.map { _ =>
            e.visitInvalid { e =>
              typecheckRec(e)
              true
            }
          }
          (Seq(future), max + 1)
        }
        else
          (fs.flatten, max + 1)
      }
    }

    val (fs, hops) = recurse(e)
    val res = Future.sequence(fs)

    if ((e.height % clusterParam) != 0) {
      val res2 = res map { _ =>
        e.visitInvalid { e =>
          typecheckRec(e)
          true
        }
      }
      (res2, trigger)
    }
    else (res, trigger)
  }
}

case class FuturisticHeightBUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new FuturisticHeightBUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}

abstract class FuturisticLevelBUChecker[CS <: ConstraintSystem[CS]] extends FuturisticBUChecker[CS] {
  override val clusterParam = 31 //TODO try to measure the best cluster size for a future

  override def bottomUpFuture(e: Node_[Result]): (Future[Any], Promise[Unit]) = {
    import collection.mutable.ArrayBuffer

    val level = Array.fill(e.height + 1)(ArrayBuffer.empty[Node_[Result]])
    e.foreach { node =>
      level(node.height) += node
    }
    val numCores = Runtime.getRuntime.availableProcessors()
    val trigger: Promise[Unit] = Promise()
    var res = trigger.future
    var size = e.size
    var i = 0
    while (i < level.length && size > clusterParam) {
      val lvl = level(i)
      val clusterSize = (lvl.size.toDouble / numCores).ceil.toInt
      val fs = for (part <- 0 until lvl.size by clusterSize)
        yield res.map { _ =>
          for (i <- part until (part + clusterSize).min(lvl.size))
            typecheckRec(lvl(i))
        }
      size -= lvl.size
      i += 1
      res = Future.fold(fs)(()) { case x => () }
    }

    if (size != 0) {
      val res2 = res map { _ =>
        e.visitInvalid { e =>
          typecheckRec(e)
          true
        }
      }
      (res2, trigger)
    }
    else (res, trigger)
  }
}

case class FuturisticLevelBUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new FuturisticLevelBUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}


object JoinBUChecker {

  final class ThreadPool {
    private var _live = false
    private val buffer = collection.mutable.ArrayBuffer[Runnable]()
    private val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())

    //private val queue = new ConcurrentLinkedQueue[Runnable]()

    def start(): Unit = {
      _live = true
      buffer.foreach(pool.execute(_))
      buffer.clear()
//      (1 to Runtime.getRuntime.availableProcessors()) foreach {
//        i =>
//          new Thread {
//            override def run() = {
//              while (_live) {
//                val thunk = queue.poll()
//                if (thunk != null) {
//                  try {
//                    thunk.run()
//                  }
//                  catch {
//                    case e: Exception => println(e)
//                  }
//                }
//              }
//            }
//          }.start()
//      }

    }

    def shutdown() = pool.shutdownNow()
    
    def stop(): Unit = {
      _live = false
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

abstract class JoinBUChecker[CS <: ConstraintSystem[CS]] extends BUChecker[CS](true) {
  import Join.Join

  val clusterParam = 4

  final def work(thunk: => Unit): Unit = work(new Runnable { def run() = thunk })
  final def work(thunk: Runnable): Unit = { JoinBUChecker.pool.submit(thunk) }

  def prepareSchedule(root: Node_[Result]): Join = {
    val typeCheckDone: Join = Join(0)

    def recurse(e: Node_[Result], parentJoin: Join): Unit = {
      if (e.height <= clusterParam) {
        parentJoin.join()
  //      println(s"${System.nanoTime()} ${Thread.currentThread().getId}: putting ${System.identityHashCode(e)} with height ${e.height} and size ${e.size}")
        work {
    //      println(s"${System.nanoTime()} ${Thread.currentThread().getId}: checking ${System.identityHashCode(e)} with height ${e.height} and size ${e.size}")
          e.visitInvalid { e =>
            typecheckRec(e)
            true
          }
          parentJoin.leave()
        }
      }
      else {
        if (e.height % clusterParam == 0) {
          val nodeJoin = Join(0)
          parentJoin.join()
          nodeJoin andThen {
      //      println(s"${System.nanoTime()} ${Thread.currentThread().getId}: putting ${System.identityHashCode(e)} with height ${e.height} and size ${e.size}")
            work {
        //      println(s"${System.nanoTime()} ${Thread.currentThread().getId}: checking ${System.identityHashCode(e)} with height ${e.height} and size ${e.size}")
            e.visitInvalid { e =>
              typecheckRec(e)
              true
            }
            parentJoin.leave()
          }
          }

          e.kids.seq.foreach { k => recurse(k, nodeJoin) }
        }
        else e.kids.seq.foreach { k => recurse(k, parentJoin) }
      }
    }

    if (root.height % clusterParam != 0) {
      val joinRoot = Join(0)
      typeCheckDone.join()
      joinRoot andThen work {
        root.visitInvalid { e =>
          typecheckRec(e)
          true
        }
        typeCheckDone.leave()
      }
      root.kids.seq.foreach { k => recurse(k, joinRoot) }
    }
    else recurse(root, typeCheckDone)
    typeCheckDone
  }

  override def typecheckImpl(e: Node): Either[T, TError] = {
    val root = e.withType[Result]
    val join = prepareSchedule(root)
    var complete = false
    join andThen {
      this.synchronized {
        complete = true
        this.notify()
      }
    }

    localState.stats(TypeCheck) {
      JoinBUChecker.pool.start()

      this.synchronized {
        while (!complete)
          this.wait()
      }
      JoinBUChecker.pool.stop()
      val (t_, reqs, sol_) = root.typ
      val sol = sol_.tryFinalize
      val t = t_.subst(sol.substitution)

      if (!reqs.isEmpty)
        Right(s"Unresolved context requirements $reqs, type $t, unres ${sol.unsolved}")
      else if (!sol.isSolved)
        Right(s"Unresolved constraints ${sol.unsolved}, type $t")
      else
        Left(t)
    }
  }
}

case class JoinBUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new JoinBUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}
