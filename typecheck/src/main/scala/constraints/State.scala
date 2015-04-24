package constraints

case class State[Type <: Typ[Type]](gen: GenBase[Type], stats: Statistics)
