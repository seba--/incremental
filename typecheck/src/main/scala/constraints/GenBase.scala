package constraints

trait GenBase {
  def freshSymbol[T](prefix: String): CVar[T]
}
