package tasks

/**
 * @author Mirko Köhler
 */
abstract class Updateable[T](init : T) {

	private var _e = init

	def apply() = get

	def get : T = _e

	def <=(newE : T) : Unit = {
		if (_e != newE) {
			_e = newE
			updated()
		}
	}

	def updated() : Unit

	implicit def toValue : T =
		get
}

