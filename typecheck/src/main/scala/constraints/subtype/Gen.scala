package constraints.subtype

import constraints.{CVar, GenBase}

class Gen extends GenBase {
  private var _pos: Set[CVar[_]] = Set()
  private var _neg: Set[CVar[_]] = Set()

  def isPositive(a: CVar[_]): Boolean = _pos(a)
  def isNegative(a: CVar[_]): Boolean = _neg(a)
  def isBipolar(a: CVar[_]): Boolean = isPositive(a) && isNegative(a)
  def isProperPositive(a: CVar[_]): Boolean = _pos(a) && !_neg(a)
  def isProperNegative(a: CVar[_]): Boolean = !_pos(a) && _neg(a)

  private var _ids = Map[String, Int]().withDefaultValue(0)
  def freshSymbol[T](prefix: String): CVar[T] = {
    val next = _ids(prefix)
    _ids = _ids + (prefix -> (next + 1))
    CVar(Symbol(prefix + next))
  }

  def freshUVar(positive: Boolean): UCName = {
    val v = UCName(freshSymbol("x$"))
    if (positive) _pos += v.x
    else _neg += v.x
    v
  }

  def freshBiVar(): UCName = {
    val res = freshUVar(true)
    _neg += res.x
    res
  }
}