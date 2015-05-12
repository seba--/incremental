package incremental.pcf.with_subtyping

import incremental.Node._
import incremental.pcf.Exp_$
import constraints.subtype._

/**
 * Created by seba on 13/11/14.
 */

// uses different type annotation subtype.Type than pcf.Abs
case object Abs extends Exp_(simple(Seq(classOf[Symbol]), Exp_.cExp) orElse simple(Seq(classOf[Symbol], classOf[Type]), Exp_.cExp) orElse (simple(Seq(classOf[Seq[Symbol]]), Exp_.cExp)))
