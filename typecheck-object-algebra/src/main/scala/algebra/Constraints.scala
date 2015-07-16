package algebra

trait Constraints[C, CS] {
  def csempty: CS
  def csmake(cs: C*): CS
  def cscompose(css: CS*): CS

  private var gensymIx: Int = 0
  def gensym: String = {
    val x = "x$" + gensymIx
    gensymIx += 1
    x
  }
}

trait UnificationTypes[T] {
  def UVar(x: String): T
}


trait EqConstraint[T, C] {
  def eqConstraint(t1: T, t2: T): C
}
