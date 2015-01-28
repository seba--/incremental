package data

import scala.collection.mutable


/**
 * @author Mirko Köhler
 */
case class Exp(kind : ExpKind, values : mutable.Seq[Any], children : mutable.Seq[Exp])

trait ExpKind
object RegExpTerminal extends ExpKind
object RegExpAlt extends ExpKind
object RegExpSeq extends ExpKind
object RegExpAsterisk extends ExpKind

