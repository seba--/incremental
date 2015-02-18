package data

import tasks.Updateable

/**
 * @author Mirko Köhler
 */
trait IList[T] extends Data {
	def toList : List[T]

	def head : IBox[T]
	def tail : IBox[IList[T]]
}

object IList {
	def apply[T](ts : T*) : IList[T] =
		if (ts.isEmpty)
			IListEmpty()
		else
			IListElement(ts.head, apply(ts.tail : _*))

	def empty[T] = IListEmpty[T]()
}

case class IListEmpty[T]() extends IList[T] {
	override def toList = Nil

	override def head = throw new NoSuchElementException("head of empty list")
	override def tail = throw new NoSuchElementException("tail of empty list")

}

class IListElement[T](h : T, t : IList[T]) extends IList[T] {
	val head = IBox(h)
	val tail = IBox(t)

	override def toList = head :: tail.toList

	override def toString =
		head.get + " :: " + tail.get
}

object IListElement {
	def apply[T](h : T, t: IList[T]) =
		new IListElement[T](h,t)

	def unapply[T](e : IListElement[T]) =
		Some(e.head.get, e.tail.get)
}







