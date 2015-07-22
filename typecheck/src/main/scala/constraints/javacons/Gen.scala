package constraints.javacons

import java.util.concurrent.atomic.AtomicInteger

import constraints.{CVar, GenBase}

/**
 * Created by qwert on 22.07.15.
 */
class Gen extends GenBase{
  private val _next = new AtomicInteger()

  @inline
  def next(): Int = _next.incrementAndGet()

  def freshSymbol[T](prefix: String): CVar[T] = CVar(Symbol(prefix + next()))
}