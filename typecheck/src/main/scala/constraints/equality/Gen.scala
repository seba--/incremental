package constraints.equality

import constraints.{CVar, GenBase}
import scala.collection.JavaConversions.mapAsScalaMap

import scala.util.Random

class Gen extends GenBase {
  private var _next = 0

  @inline
  def next(): Int = {
    val n = _next
    _next += 1
    n
  }

  def freshSymbol[T](prefix: String): CVar[T] =
    CVar(Symbol(prefix + next()))
}
