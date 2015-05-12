package incremental.pcf


import scala.language.postfixOps
import constraints.Statistics
import constraints.equality.{ConstraintSystemFactory, ConstraintSystem}
import scala.concurrent.{ExecutionContext, Await, Promise, Future}
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import incremental.Node.Node
import incremental._

/**
 * Created by seba on 13/11/14.
 */
abstract class FuturisticBUChecker[CS <: ConstraintSystem[CS]] extends BUChecker[CS] {
  val clusterParam = 4
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
        val (fs, nodecounts) = (e.mapKids { k => recurse(k) }).unzip
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

    Util.timed(localState -> Statistics.typecheckTime) {
      trigger success (Unit)
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

abstract class FuturisticHeightBUChecker[CS <: ConstraintSystem[CS]] extends FuturisticBUChecker[CS] {
  override val clusterParam = 4

  override def bottomUpFuture(e: Node_[Result]): (Future[Any], Promise[Unit]) = {
    val trigger: Promise[Unit] = Promise()
    val fut = trigger.future
    def recurse(e: Node_[Result]): (Future[Any], Int) = {
      if (e.height == clusterParam) {
        val f = fut map { _ =>
          e.visitInvalid { e =>
            typecheckRec(e)
            true
          }
        }
        (f, 1)
      }
      else {
        val (fs, hops) = (e.mapKids { k => recurse(k) }).unzip
        val max = hops.foldLeft(0) { case (i,j) => i.max(j) }
        val join = Future.fold(fs)(()) { case x => () }

        if ((max % clusterParam) == 0) {
          val future = join.map { _ =>
            e.visitInvalid { e =>
              typecheckRec(e)
              true
            }
          }
          (future, max + 1)
        }
        else
          (join, max + 1)
      }
    }

    val (res, hops) = recurse(e)

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

abstract class FuturisticHeightListBUChecker[CS <: ConstraintSystem[CS]] extends FuturisticHeightBUChecker[CS] {

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
        val (fs, hops) = (e.mapKids { k => recurse(k) }).unzip
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

case class FuturisticHeightListBUCheckerFactory[CS <: ConstraintSystem[CS]](factory: ConstraintSystemFactory[CS]) extends TypeCheckerFactory[CS] {
  def makeChecker = new FuturisticHeightListBUChecker[CS] {
    type CSFactory = factory.type
    implicit val csFactory: CSFactory = factory
  }
}
