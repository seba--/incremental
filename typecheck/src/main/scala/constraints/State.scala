package constraints

case class State[V <: Type, G <: GenBase[V]](gen: G, stats: Statistics)
