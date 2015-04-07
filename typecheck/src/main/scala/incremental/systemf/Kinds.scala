package incremental.systemf

import incremental.Exp._
import incremental.Type
import incremental.{SimpleSyntax, ExpKind}

/**
 * Created by seba on 13/11/14.
 */

case object Num  extends ExpKind(simple(0, classOf[Integer]))
case object Add  extends ExpKind(simple(2))
case object Mul  extends ExpKind(simple(2))
case object Var  extends ExpKind(simple(0, classOf[Symbol]))
case object Abs  extends ExpKind(simple(1, classOf[Symbol]) orElse simple(1, classOf[Symbol], classOf[Type]))
case object TAbs extends ExpKind(simple(1, classOf[Symbol]))
case object App  extends ExpKind(simple(2))
case object TApp extends ExpKind(simple(1, classOf[Type]))
case object If0  extends ExpKind(simple(3))
case object Fix  extends ExpKind(simple(1))
