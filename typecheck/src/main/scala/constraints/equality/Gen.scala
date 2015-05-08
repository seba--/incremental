package constraints.equality

import constraints.{CVar, GenBase}

import scala.util.Random

class Gen extends GenBase {
  //private var _ids = Map[String, Int]().withDefaultValue(0)
  def freshSymbol[T](prefix: String): CVar[T] = {
   /* val next = _ids(prefix)
    _ids = _ids + (prefix -> (next + 1))
    CVar(Symbol(prefix + next))*/
    CVar(Symbol(prefix + Random.nextLong()))
  }
}
