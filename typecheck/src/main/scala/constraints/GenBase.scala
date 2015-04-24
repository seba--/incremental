package constraints

trait GenBase[V <: Type] {
  def freshUVar(): V
}
