package algebra

trait Requirements[CS, R, RS] {
  def rsempty: RS
  def rsmake(rs: R*): RS
  def rscompose(rss: RS*): (RS, CS)
  def rssatisfy(rs: RS, r: R): (RS, CS)
}

trait TypedVarBinding[T, R] {
  def typedVarBinding(x: String, t: T): R
}
