package incremental.pcf


import java.util.concurrent.Executors

import scala.language.postfixOps
import constraints.StatKeys._
import constraints.equality.{ConstraintSystemFactory, ConstraintSystem}
import scala.concurrent.{ExecutionContext, Await, Promise, Future}
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import incremental.Node.Node
import incremental._

object FuturisticBUChecker {
  //implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors()))
 // implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.global
}

import FuturisticBUChecker._

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

