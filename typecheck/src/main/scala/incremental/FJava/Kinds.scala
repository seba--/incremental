package incremental.FJava

import incremental.ExpKind

/**
 * Created by lirakuci on 3/10/15.
 */

case object Var extends ExpKind(0)
case object Field extends ExpKind(1)
case object Method extends ExpKind(2)
case object New extends ExpKind(1)
case object UCast extends ExpKind(1)
case object DCast extends ExpKind(1)