package lang

import algebra._

trait IntExps[E] {
  def lit(n: Int): E
  def add(e1: E, e2: E): E
  def mul(e1: E, e2: E): E
  def if0(c: E, t: E, e: E): E
}

trait IntTypes[T] {
  def TInt: T
}

trait IntEval extends IntExps[Int] {
  type Dom = Int
  override def lit(n: Dom) = n
  override def add(e1: Dom, e2: Dom) = e1 + e2
  override def mul(e1: Dom, e2: Dom) = e1 * e2
  override def if0(c: Dom, t: Dom, e: Dom) = if (c == 0) t else e
}

trait IntTyping[T, C, CS, R, RS, CT] extends IntExps[CT] {
  val algIntTypes: IntTypes[T]
  val algConstraints: Constraints[C, CS]
  val algEqConstraint: EqConstraint[T, C]
  val algRequirements: Requirements[CS, R, RS]
  val algCoTypes: Cotype[T, CS, RS, CT]
  val cotypeOps = Cotype.cotypeOps(algCoTypes)

  import algIntTypes._
  import algConstraints._
  import algEqConstraint._
  import algRequirements._
  import algCoTypes._
  import cotypeOps._

  override def lit(n: Int) = cotype(TInt, csempty, rsempty)

  override def add(e1: CT, e2: CT) = {
    val (mrs, mcs) = rscompose(e1.rs, e1.rs)
    val newcs = csmake(eqConstraint(e1.t, TInt), eqConstraint(e2.t, TInt))
    val allcs = cscompose(e1.cs, e2.cs, mcs, newcs)
    cotype(TInt, allcs, mrs)
  }

  override def mul(e1: CT, e2: CT) = add(e1, e2)

  override def if0(c: CT, t: CT, e: CT) = {
    val (mrs, mcs) = rscompose(c.rs, t.rs, e.rs)
    val newcs = csmake(eqConstraint(c.t, TInt), eqConstraint(t.t, e.t))
    val allcs = cscompose(c.cs, t.cs, e.cs, mcs, newcs)
    cotype(t.t, allcs, mrs)
  }
}
