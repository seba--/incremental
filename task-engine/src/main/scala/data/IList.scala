package data

/**
 * @author Mirko Köhler
 */
trait IList[T]extends Data {
	def isEmpty : Boolean


}

class IListElement[T](h : T, t : IList[T]) extends IList[T] {
	val isEmpty = false

	private var _head: T = h
	private var _tail: IList[T] = t

	object head {
		def apply() = _head

		def <=(h: T) {
			if (h != head) {
				_dirty = true
				_head = h
			}
		}
	}

	object tail {
		def apply() = _tail

		def <=(t: IList[T]): Unit = {
			if (t != _tail) {
				_dirty = true
				_tail = t
			}
		}
	}

	override def toString =
		head() + " :: " + tail()
}

object IListElement {
	def apply[T](h : T, t: IList[T]) =
		new IListElement[T](h,t)

	def unapply[T](e : IListElement[T]) =
		Some(e.head(), e.tail())
}

case class IListEmpty[T]() extends IList[T] {
	val isEmpty = true
}





