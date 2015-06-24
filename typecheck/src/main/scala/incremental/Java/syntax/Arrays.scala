package incremental.Java.syntax

import incremental.{NodeKind, SyntaxChecking}
import incremental.Java.syntax.JavaSyntaxChecker._
import incremental.Node._

/**
 * Created by qwert on 06.05.15.
 */


trait NT_ArrayInit extends NT_VarInit
case object ArrayInit extends NodeKind(noLits andAlso unsafeAllKids(classOf[NT_VarInit])) with NT_ArrayInit
