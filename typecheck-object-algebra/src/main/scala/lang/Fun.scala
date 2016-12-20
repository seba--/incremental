package lang

import algebra._

trait FunExps[E] {
  def va(x: String): E
  def abs(x: String, e: E): E
  def app(e1: E, e2: E): E
}

trait FunTypes[T] {
  def TFun(from: T, to: T): T
}

object FunEval {
  case class FunVal(x: String, e: Env => FunVal, env: Env)
  type Env = Map[String, FunVal]
  type Dom = Env => FunVal
}
trait FunEval extends FunExps[FunEval.Dom] {
  import FunEval._

  override def va(x: String) = env => env(x)
  override def abs(x: String, e: Dom) = env => FunVal(x, e, env)
  override def app(e1: Dom, e2: Dom) = env => (e1(env), e2(env)) match {
    case (FunVal(x, e, fenv), v) => e(fenv + (x -> v))
  }
}

trait FunTyping[T, C, CS, R, RS, CT] extends FunExps[CT] {
  val algIntTypes: FunTypes[T] with UnificationTypes[T]
  val algConstraints: Constraints[C, CS]
  val algEqConstraint: EqConstraint[T, C]
  val algRequirements: Requirements[CS, R, RS]
  val algTypedVarBinding: TypedVarBinding[T, R]
  val algCoTypes: Cotype[T, CS, RS, CT]
  val cotypeOps = Cotype.cotypeOps(algCoTypes)

  import algIntTypes._
  import algConstraints._
  import algEqConstraint._
  import algRequirements._
  import algTypedVarBinding._
  import algCoTypes._
  import cotypeOps._

  override def va(x: String) = {
    val t = UVar(gensym)
    val rs = rsmake(typedVarBinding(x, t))
    cotype(t, csempty, rs)
  }

  override def app(e1: CT, e2: CT) = {
    val (mrs, mcs) = rscompose(e1.rs, e2.rs)
    val tres = UVar(gensym)
    val newcs = csmake(eqConstraint(e1.t, TFun(e2.t, tres)))
    val allcs = cscompose(e1.cs, e2.cs, mcs, newcs)
    cotype(tres, allcs, mrs)
  }

  override def abs(x: String, e: CT) = {
    val targ = UVar(gensym)
    val (satrs, satcs)  = rssatisfy(e.rs, typedVarBinding(x, UVar(gensym)))
    val allcs = cscompose(e.cs, satcs)
    cotype(TFun(targ, e.t), allcs, satrs)
  }
}