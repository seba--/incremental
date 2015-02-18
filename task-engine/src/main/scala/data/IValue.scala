package data

/**
 * @author Mirko Köhler
 */
class IValue[T](value : T) extends Data {
	val v = IBox(value)

	override def equals(o : Any) =
		o.isInstanceOf[IValue[T]] && o.asInstanceOf[IValue[T]].v == v


}

object IValue {
	def apply[T](value : T) : IValue[T] =
		new IValue(value)
}
