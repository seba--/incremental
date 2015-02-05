package data

/**
 * @author Mirko Köhler
 */
trait IList[T]extends Data {
	def isEmpty : Boolean


}

case class IListElement[T](h : T, t : IList[T]) extends IList[T] {
	val isEmpty = false

	private var _head: T = h
	private var _tail: IList[T] = t

	object head {
		def apply() = _head

		def update(h: T) {
			if (h != head) {
				_dirty = true
				_head = h
			}
		}
	}

	object tail {
		def apply() = _tail

		def update(t: IList[T]): Unit = {
			if (t != _tail) {
				_dirty = true
				_tail = t
			}
		}
	}



}

case class IListEmpty[T]() extends IList[T] {
	val isEmpty = true
}


