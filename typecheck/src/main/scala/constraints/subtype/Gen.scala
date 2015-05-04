package constraints.subtype

import constraints.{CVar, GenBase}

class Gen extends GenBase {
  private var _pos: Set[CVar] = Set()
  private var _neg: Set[CVar] = Set()

  def isPositive(a: CVar): Boolean = _pos(a)
  def isNegative(a: CVar): Boolean = _neg(a)
  def isBipolar(a: CVar): Boolean = isPositive(a) && isNegative(a)
  def isProperPositive(a: CVar): Boolean = _pos(a) && !_neg(a)
  def isProperNegative(a: CVar): Boolean = !_pos(a) && _neg(a)

  private var _ids = Map[String, Int]().withDefaultValue(0)
  def freshSymbol(prefix: String): CVar = {
    val next = _ids(prefix)
    _ids = _ids + (prefix -> (next + 1))
    CVar(Symbol(prefix + next))
  }

  def freshUVar(positive: Boolean): UVar = {
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