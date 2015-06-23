package util

import java.util.concurrent.atomic.AtomicInteger

/**
 * Joins which immediately trigger at completion time
 */
object Join {
  final class Join(size: Int) {
    private val counter = new AtomicInteger(size)
    private[Join] var kont: () => Unit = () => ()

    @inline
    def andThen(k: => Unit) = {
      kont = () => k
    }

    @inline
    def join() = counter.incrementAndGet()

    @inline
    def leave() = {
      val i = counter.decrementAndGet()
      if (i == 0)
        kont()
      else if (i < 0)
        throw new IllegalStateException("Join already completed")
    }
  }

  @inline
  def when(j: Join)(k: => Unit) = j andThen k
  def apply(size: Int): Join = new Join(size)
}

/**
 * Joins with completion and trigger separated in order to avoid
 * "the slowest thread gets all the work" scenarios. Multiple threads
 * may compete over the reaction body of a completed join via a method. The implementation
 * ensures that exactly one threads gets to execute the trigger.
 */
object Jn {
  type Thunk = () => Unit
  final class Join[T](p: Join[T], size: Int) {
    private val counter = new AtomicInteger(size)
    private[Join] var kont: T = _

    val parent = Option(p)

    @inline
    def andThen(k: T) = {
      kont = k
    }

    @inline
    def join() = counter.incrementAndGet()

    @inline
    def probe() = counter.get()

    @inline
    def leave(): Int = {
      val i = counter.decrementAndGet()
      if (i < 0)
        throw new IllegalStateException("Join already completed")
      i
    }

    @inline
    def tryClaim(): Option[T] = {
      if (counter.compareAndSet(0, -1) && kont != null)
        Some(kont)
      else
        None
    }
  }

  @inline
  def apply[T](parent: Join[T], size: Int): Join[T] = new Join[T](parent, size)
  def apply[T](parent: Join[T]): Join[T] = this(parent, 0)
}