package constraints

case class State[V <: Type](gen: GenBase[V], stats: Statistics)
