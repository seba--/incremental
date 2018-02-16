package incremental.typeclasses

import constraints.CVar
import constraints.equality.CSubst.CSubst
import constraints.equality._

// body[alpha := substitute] = result
case class EqSubstConstraint(body: Type, alpha: Symbol, alphaIsInternal: Boolean, substitute: Type, result: Type) extends Constraint {
  private def withResult[CS <: ConstraintSystem[CS]](t: Type, cs: CS) : CS = t.unify(result, cs)

  private def substAlpha(s: CSubst) =
    if (!alphaIsInternal) (alpha, false)
    else s.hget(CVar[Type](alpha)) match {
      case Some(TVar(beta)) => (beta, false)
      case Some(UVar(CVar(beta))) => (beta, true)
      case None => (alpha, alphaIsInternal)
      case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
    }

  def solve[CS <: ConstraintSystem[CS]](cs: CS): CS = {
    val tbody = body.subst(cs.substitution)
    val (beta, betaIsInternal) = substAlpha(cs.substitution)

    tbody match {
      case TVar(`beta`) | UVar(CVar(`beta`)) => withResult(substitute, cs)
      case TVar(_) if !betaIsInternal => withResult(tbody, cs) // because alpha is user-defined and different

      case TNum => withResult(TNum, cs)
      case TFun(t1, t2) =>
        val X = UVar(cs.gen.freshSymbol("x$"))
        val Y = UVar(cs.gen.freshSymbol("x$"))
        val cons1 = EqSubstConstraint(t1, beta, betaIsInternal, substitute, X)
        val cons2 = EqSubstConstraint(t2, beta, betaIsInternal, substitute, Y)
        withResult(TFun(X, Y), cons1.solve(cons2.solve(cs)))
      case TUniv(`beta`, _) => withResult(tbody, cs)
      case TUniv(gamma, t) if !betaIsInternal =>
        val X = UVar(cs.gen.freshSymbol("x$"))
        val tcons = EqSubstConstraint(t, beta, betaIsInternal, substitute, X)
        withResult(TUniv(gamma, X), tcons.solve(cs))

      // either:
      // - tbody == TVarInternal   <== Example: x. (x [Num]) + (x [Num -> Num])
      // - tbody == TVar && betaIsInternal ==> cannot happen, type argument always has to become concrete
      // - tbody == TUnivInternal ==> cannot happen, universal types have explicit argument in TAbs('alpha, t)
      // - tbody == TUni && betaIsInternal ==> cannot happen, type argument always has to become concrete
      case _ => cs.notyet(EqSubstConstraint(tbody, beta, betaIsInternal, substitute, result))
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](cs: CS) = solve(cs)

  def subst(s: CSubst) = {
    val (newalpha, newalphaIsInternal) = substAlpha(s)
    EqSubstConstraint(body.subst(s), newalpha, newalphaIsInternal, substitute.subst(s), result.subst(s))
  }
}