package constraints.equality

import constraints.GenBase

class Gen[CS <: ConstraintSystem[CS]](csFactory: ConstraintSystemFactory[CS]) extends GenBase[UVar[CS]] {
  type V = UVar[CS]
  private var _nextId = 0
  def freshUVar(): UVar[CS] = {
    val v = UVar[CS](Symbol("x$" + _nextId))(csFactory)
    _nextId += 1
    v
  }
}
