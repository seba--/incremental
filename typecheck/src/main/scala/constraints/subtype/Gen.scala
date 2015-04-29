package constraints.subtype

import constraints.GenBase

class Gen extends GenBase[UVar] {
  private var _pos: Set[Symbol] = Set()
  private var _neg: Set[Symbol] = Set()

  def isPositive(a: Symbol): Boolean = _pos(a)
  def isNegative(a: Symbol): Boolean = _neg(a)
  def isBipolar(a: Symbol): Boolean = isPositive(a) && isNegative(a)
  def isProperPositive(a: Symbol): Boolean = _pos(a) && !_neg(a)
  def isProperNegative(a: Symbol): Boolean = !_pos(a) && _neg(a)

  private var _nextId = 0
  def freshUVar(positive: Boolean): UVar = {
    val v = UVar(Symbol("x$" + _nextId))
    _nextId += 1
    if (positive) _pos += v.x
    else _neg += v.x
    v
  }
  def freshBiVar(): UVar = {
    val res = freshUVar(true)
    _neg += res.x
    res
  }

  def freshUVar() = freshUVar(true)
}