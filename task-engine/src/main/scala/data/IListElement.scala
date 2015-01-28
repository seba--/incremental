package data

/**
 * @author Mirko Köhler
 */
trait IList {
	def isEmpty : Boolean
	def head : Any
	def tail : IList
}

case class IListElement(var head : Any, var tail : IList) extends IList {
	val isEmpty = false
}

case object IListEmpty extends IList {
	val isEmpty = true
	def head = throw new IllegalStateException("Can not get head of empty list.")
	def tail = throw new IllegalStateException("Can not get tail of empty list.")
}


