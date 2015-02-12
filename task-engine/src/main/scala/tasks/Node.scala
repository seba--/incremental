package tasks

/**
 * @author Mirko Köhler
 */
trait Node {
	class UpdateableValue[T](init : T) extends Updateable[T](init) {
		def updated() =
			_dirty = true
	}
	
	protected var _dirty = true

	def visited() {
		_dirty = false
	}

	def isDirty = _dirty
}
