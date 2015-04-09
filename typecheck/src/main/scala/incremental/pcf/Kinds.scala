package incremental.pcf

import incremental.Node._
import incremental.NodeKind
import incremental.Typ

/**
 * Created by seba on 13/11/14.
 */

case object Num extends NodeKind(simple(0, classOf[Integer]))
case object Add extends NodeKind(simple(2))
case object Mul extends NodeKind(simple(2))
case object Var extends NodeKind(simple(0, classOf[Symbol]))
case object Abs extends NodeKind(simple(1, classOf[Symbol]) orElse simple(1, classOf[Symbol], classOf[Typ[_]]))
case object App extends NodeKind(simple(2))
case object If0 extends NodeKind(simple(3))
case object Fix extends NodeKind(simple(1))
