package constraints.equality

import constraints.GenBase

class Gen extends GenBase[Type] {
  type V = UVar
  private var _nextId = 0
  def freshUVar(): UVar = {
    val v = UVar(Symbol("x$" + _nextId))
    _nextId += 1
    v
  }
}
