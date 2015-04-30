package incremental.systemf

import constraints.equality.Type.Companion.TSubst
import constraints.equality._

case class EqSubstConstraint(body: Type, alpha: Symbol, alphaIsInternal: Boolean, substitute: Type, result: Type) extends Constraint {
  private def withResult[CS <: ConstraintSystem[CS]](t: Type, s: ConstraintSystem[CS], csf: ConstraintSystemFactory[CS]) : CS
  = t.unify(result, s.substitution)(csf)

  private def substAlpha(s: TSubst) =
    if (!alphaIsInternal) (alpha, false)
    else s.get(alpha) match {
      case Some(TVar(beta)) => (beta, false)
      case Some(UVar(beta)) => (beta, true)
      case None => (alpha, alphaIsInternal)
      case Some(_) => throw new IllegalArgumentException(s"Cannot replace type bound by non-variable type")
    }

  def solve[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS], csf: ConstraintSystemFactory[CS]): CS = {
    val tbody = body.subst(s.substitution)
    val (beta, betaIsInternal) = substAlpha(s.substitution)

    tbody match {
      case TVar(`beta`) | UVar(`beta`) => withResult(substitute, s, csf)
      case TVar(_) if !betaIsInternal => withResult(tbody, s, csf) // because alpha is user-defined and different

      case TNum => withResult(TNum, s, csf)
      case TFun(t1, t2) =>
        val X = csf.state.value.gen.freshUVar()
        val Y = csf.state.value.gen.freshUVar()
        (EqSubstConstraint(t1, beta, betaIsInternal, substitute, X).solve(s, csf) mergeSubsystem
          EqSubstConstraint(t2, beta, betaIsInternal, substitute, Y).solve(s, csf) mergeSubsystem
          withResult(TFun(X, Y), s, csf))
      case TUniv(`beta`, _) => withResult(tbody, s, csf)
      case TUniv(gamma, t) if !betaIsInternal =>
        val X = csf.state.value.gen.freshUVar()
        (EqSubstConstraint(t, beta, betaIsInternal, substitute, X).solve(s, csf) mergeSubsystem
          withResult(TUniv(gamma, X), s, csf))

      // either:
      // - tbody == TVarInternal   <== Example: x. (x [Num]) + (x [Num -> Num])
      // - tbody == TVar && betaIsInternal ==> cannot happen, type argument always has to become concrete
      // - tbody == TUnivInternal ==> cannot happen, universal types have explicit argument in TAbs('alpha, t)
      // - tbody == TUni && betaIsInternal ==> cannot happen, type argument always has to become concrete
      case _ => csf.notyet(EqSubstConstraint(tbody, beta, betaIsInternal, substitute, result))
    }
  }

  def finalize[CS <: ConstraintSystem[CS]](s: ConstraintSystem[CS], csf: ConstraintSystemFactory[CS]) = solve(s, csf)

  def subst(s: TSubst) = {
    val (newalpha, newalphaIsInternal) = substAlpha(s)
    EqSubstConstraint(body.subst(s), newalpha, newalphaIsInternal, substitute.subst(s), result.subst(s))
  }
}