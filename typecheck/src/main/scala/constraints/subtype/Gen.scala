package constraints.subtype

import constraints.GenBase

class Gen extends GenBase {
  private var _pos: Set[Symbol] = Set()
  private var _neg: Set[Symbol] = Set()

  def isPositive(a: Symbol): Boolean = _pos(a)
  def isNegative(a: Symbol): Boolean = _neg(a)
  def isBipolar(a: Symbol): Boolean = isPositive(a) && isNegative(a)
  def isProperPositive(a: Symbol): Boolean = _pos(a) && !_neg(a)
  def isProperNegative(a: Symbol): Boolean = !_pos(a) && _neg(a)

  private var _ids = Map[String, Int]().withDefaultValue(0)
  def freshSymbol(prefix: String): Symbol = {
    val next = _ids(prefix)
    _ids = _ids + (prefix -> (next + 1))
    Symbol(prefix + next)
  }

  def freshUVar(positive: Boolean = true): UVar = {
    val v = UVar(freshSymbol("x$"))
    if (positive) _pos += v.x
    else _neg += v.x
    v
  }

  def freshBiVar(): UVar = {
    val res = freshUVar(true)
    _neg += res.x
    res
  }
}