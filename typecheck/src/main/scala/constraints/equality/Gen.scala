package constraints.equality

import java.util.concurrent.atomic.AtomicInteger

import constraints.{CVar, GenBase}


class Gen extends GenBase {
  private var _next = new AtomicInteger()

  @inline
  def next(): Int = _next.incrementAndGet()


  def freshSymbol[T](prefix: String): CVar[T] =
    CVar(Symbol(prefix + next()))
}
