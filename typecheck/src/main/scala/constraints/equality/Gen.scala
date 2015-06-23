package constraints.equality

import java.util.concurrent.atomic.AtomicInteger

import constraints.{CVar, GenBase}


class Gen extends GenBase {
  private val _next = new ThreadLocal[AtomicInteger]() {
    override def initialValue = new AtomicInteger(0)
  }

  @inline
  def next(): Int = _next.get().incrementAndGet()

  def freshSymbol[T](prefix: String): CVar[T] =
    CVar(Symbol(s"$prefix:${Thread.currentThread().getId}:${next()}"))
}
