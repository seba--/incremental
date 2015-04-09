package incremental.pcf_graph

import incremental.NodeKind

/**
 * Created by seba on 13/11/14.
 */

case object Num extends NodeKind
case object Add extends NodeKind
case object Mul extends NodeKind
case object Var extends NodeKind
case object Abs extends NodeKind
case object App extends NodeKind
case object If0 extends NodeKind
case object Fix extends NodeKind
