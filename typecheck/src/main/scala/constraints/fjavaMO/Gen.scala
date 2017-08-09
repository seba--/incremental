package constraints.fjavaMO

import java.util.concurrent.atomic.AtomicInteger

import constraints.{CVar, GenBase}
import incremental.fjavaMO.UCName

class Gen extends GenBase {
  private var _pos: Set[CVar[_]] = Set()
  private var _neg: Set[CVar[_]] = Set()

  def isPositive(a: CVar[_]): Boolean = _pos(a)
  def isNegative(a: CVar[_]): Boolean = _neg(a)
  def isBipolar(a: CVar[_]): Boolean = isPositive(a) && isNegative(a)
  def isProperPositive(a: CVar[_]): Boolean = _pos(a) && !_neg(a)
  def isProperNegative(a: CVar[_]): Boolean = !_pos(a) && _neg(a)

  private val _next = new AtomicInteger()

  @inline
  def next(): Int = _next.incrementAndGet()


  def freshSymbol[T](prefix: String): CVar[T] =
    CVar(Symbol(prefix + next()))

  def freshUCName(positive: Boolean): UCName = {
    val v = UCName(freshSymbol("x$"))
    if (positive) _pos += v.x
    else _neg += v.x
    v
  }

  def freshBiVar(): UCName = {
    val res = freshUCName(true)
    _neg += res.x
    res
  }
}