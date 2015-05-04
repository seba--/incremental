package constraints.normequality

import constraints.GenBase

class Gen extends GenBase {
  private var _ids = Map[String, Int]().withDefaultValue(0)
  def freshSymbol(prefix: String): Symbol = {
    val next = _ids(prefix)
    _ids = _ids + (prefix -> (next + 1))
    Symbol(prefix + next)
  }
}
