package tasks

/**
 * @author Mirko Köhler
 */
trait Updateable[T] {

	def apply() = get

	def get : T
	def <=(t : T) : Unit

	implicit def toValue : T =
		get
}

