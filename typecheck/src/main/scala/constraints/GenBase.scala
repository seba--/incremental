package constraints

trait GenBase[Type <: Typ[Type]] {
  type V <: Type
  def freshUVar(): V
}
