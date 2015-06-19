package util

import java.util.concurrent.atomic.AtomicInteger

/**
 * Created by oliver on 07.06.15.
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

object Jn {
  type Thunk = () => Unit
  final class Join(p: Join, size: Int) {
    private val counter = new AtomicInteger(size)
    private[Join] var kont: Thunk = () => ()

    val parent = Option(p)

    @inline
    def andThen(k: => Unit) = {
      kont = () => k
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
    def tryClaim(): Option[Thunk] = {
      if (counter.compareAndSet(0, -1))
        Some(kont)
      else
        None
    }
  }

  @inline
  def when(j: Join)(k: => Unit) = j andThen k
  def apply(parent: Join, size: Int): Join = new Join(parent, size)
  def apply(parent: Join) = new Join(parent, 0)
}