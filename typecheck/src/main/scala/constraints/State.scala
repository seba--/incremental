package constraints

case class State[T <: Type](gen: GenBase[T], stats: Statistics)
