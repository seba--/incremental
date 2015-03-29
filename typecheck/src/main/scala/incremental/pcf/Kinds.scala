package incremental.pcf

import incremental.ExpKind

/**
 * Created by seba on 13/11/14.
 */

case object Num extends ExpKind(0)
case object Add extends ExpKind(2)
case object Mul extends ExpKind(2)
case object Var extends ExpKind(0)
case object Abs extends ExpKind(1)
case object App extends ExpKind(2)
case object If0 extends ExpKind(3)
case object Fix extends ExpKind(1)
case object Field extends ExpKind(2)