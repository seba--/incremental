package constraints

case class State[G <: GenBase](gen: G, stats: Statistics)
