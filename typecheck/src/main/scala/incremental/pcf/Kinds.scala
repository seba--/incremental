package incremental.pcf

import incremental.{Syntax, ExpKind}

/**
 * Created by seba on 13/11/14.
 */

case object Num extends ExpKind(Syntax(0))
case object Add extends ExpKind(Syntax(2))
case object Mul extends ExpKind(Syntax(2))
case object Var extends ExpKind(Syntax(0, Symbol.getClass))
case object Abs extends ExpKind(Syntax(1, Symbol.getClass))
case object App extends ExpKind(Syntax(2))
case object If0 extends ExpKind(Syntax(3))
case object Fix extends ExpKind(Syntax(1))
