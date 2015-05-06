package incremental.Java.syntax

import incremental.{NodeKind, SyntaxChecking}

/**
 * Created by qwert on 06.05.15.
 */

abstract class ArrayInit(syntaxcheck: SyntaxChecking.SyntaxCheck) extends NodeKind(syntaxcheck)
object ArrayInit {
  val cArrayInit = classOf[ArrayInit]
}

// Array Initialization
case object ArrayInitialize extends Expr(_ => ArrayInitSyntax)