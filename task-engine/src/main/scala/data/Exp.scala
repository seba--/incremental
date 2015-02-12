package data

import tasks.Updateable

import scala.collection.mutable


/**
 * @author Mirko Köhler
 */
class Exp(k : ExpKind, val values : mutable.Seq[Any], val children : mutable.Seq[Exp]) extends Data {

	object kind extends UpdateableValue[ExpKind](k)
}

trait ExpKind
object RegExpTerminal extends ExpKind
object RegExpAlt extends ExpKind
object RegExpSeq extends ExpKind
object RegExpAsterisk extends ExpKind

