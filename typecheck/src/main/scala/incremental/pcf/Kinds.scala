package incremental.pcf

import incremental.ExpKind

/**
 * Created by seba on 13/11/14.
 */

case object Num extends ExpKind
case object Add extends ExpKind
case object Mul extends ExpKind
case object Var extends ExpKind
case object Abs extends ExpKind
case object App extends ExpKind
case object If0 extends ExpKind
case object Fix extends ExpKind
