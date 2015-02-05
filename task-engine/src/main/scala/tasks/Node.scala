package tasks

/**
 * @author Mirko Köhler
 */
trait Node {
	protected var _dirty = true

	def visited() {
		_dirty = false
	}

	def isDirty = _dirty
}
