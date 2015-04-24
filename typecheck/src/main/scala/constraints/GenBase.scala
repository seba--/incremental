package constraints

trait GenBase[T <: Type] {
  type V <: T
  def freshUVar(): V
}
