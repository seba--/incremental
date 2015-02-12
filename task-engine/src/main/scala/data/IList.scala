package data

import tasks.Updateable

/**
 * @author Mirko Köhler
 */
trait IList[T]extends Data {
	def isEmpty : Boolean


}

class IListElement[T](h : T, t : IList[T]) extends IList[T] {
	val isEmpty = false

	val head = new UpdateableValue[T](h)
	val tail = new UpdateableValue[IList[T]](t)

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





