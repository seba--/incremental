package data

import tasks.Updateable

import scala.collection.mutable


/**
 * @author Mirko Köhler
 */
class Exp(k : ExpKind, val values : mutable.Seq[Any], val children : mutable.Seq[Exp]) extends Data {

	private var _kind = k

	object kind extends Updateable[ExpKind] {
		override def get: ExpKind = _kind

		override def <=(t: ExpKind): Unit =
			if (t != _kind) {
				_kind = t
				_dirty = true
			}
	}
}

trait ExpKind
object RegExpTerminal extends ExpKind
object RegExpAlt extends ExpKind
object RegExpSeq extends ExpKind
object RegExpAsterisk extends ExpKind

