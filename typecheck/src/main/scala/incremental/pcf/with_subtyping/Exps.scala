package incremental.pcf.with_subtyping

import constraints.equality
import incremental.{Node_, Context}
import incremental.Node._
import incremental.pcf.Exp
import incremental.pcf.PCFCheck._
import constraints.subtype._

/**
 * Created by seba on 13/11/14.
 */

// uses different type annotation subtype.Type than pcf.Abs
case object Abs extends Exp(simple(Seq(classOf[Symbol]), Exp.cExp) orElse simple(Seq(classOf[Symbol], classOf[Type]), Exp.cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), Exp.cExp))){
  def check(lits: Seq[Any], kids: Seq[Node_[equality.Constraint, _, (equality.Type, Reqs)]], context: Context[equality.Constraint]): (equality.Type, Reqs) = ???
}
