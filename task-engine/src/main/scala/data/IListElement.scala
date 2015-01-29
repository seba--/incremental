package data

/**
 * @author Mirko Köhler
 */
trait IList {
	def isEmpty : Boolean
	def head : Any
	def tail : IList

	def updateHead(h : Any)
	def updateTail(t : IList)
}

case class IListElement(var head : Any, var tail : IList) extends IList {
	val isEmpty = false

	override def updateHead(h: Any): Unit = head = h

	override def updateTail(t: IList): Unit = tail = t
}

case object IListEmpty extends IList {
	val isEmpty = true
	def head = throw new IllegalStateException("Can not get head of empty list.")
	def tail = throw new IllegalStateException("Can not get tail of empty list.")

	override def updateHead(h: Any): Unit = throw new UnsupportedOperationException("Can not update head of empty list.")
	override def updateTail(t: IList): Unit = throw new UnsupportedOperationException("Can not update tail of empty list.")
}


