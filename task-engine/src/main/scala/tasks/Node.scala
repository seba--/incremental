package tasks

import data.IValue

/**
 * @author Mirko Köhler
 */
trait Node {
	protected case class IBox[T](init : T) extends Updateable[T](init) {
		def updated() =
			_changed = true

		def toData =
			IValue(init)
	}
	
	protected var _changed = true

	def visited() {
		_changed = false
	}

	def hasChanged = _changed
}
