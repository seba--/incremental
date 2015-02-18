package tasks

import data.{IListElement, IListEmpty, IList, IValue}

/**
 * @author Mirko Köhler
 */
class TaskFoo4(val acc : IValue[Int], val list : IList[Int]) extends Task[Int](acc, list){

	override protected def internalTraverse() : Iterable[Task[_]] = (acc, list) match {
		case (_, IListEmpty()) =>
			children.clear()
			children
		case (_, IListElement(head, tail)) if
		children.count == 1
			&& children(0).isInstanceOf[TaskFoo4]
			&& children(0).asInstanceOf[TaskFoo4].acc.v.get == acc.v.get + head
			&& children(0).asInstanceOf[TaskFoo4].list == tail
		=>
			//do nothing
			children
		case (_, IListElement(head, tail)) =>
			children.clear()
			children += new TaskFoo4(IValue(acc.v.get + head), tail)
			children
		case _ => super.internalTraverse()
	}

	override protected def internalRecompute() : Unit = list match {
		case IListEmpty() =>
			result <= acc.v
		case IListElement(head, tail) =>
			result <= children(0).result.get.asInstanceOf[Int]
		case _ => super.internalRecompute()
	}


	def canBeRecomputed : Boolean = list match {
		case IListEmpty() => true
		case IListElement(head, tail) => children.count == 1 //&& !children(0).isDirty
	}
}

object TaskFoo4 {
	def foo4(acc: Int, l: List[Int]): Int =  l match {
		case Nil => acc
		case hd::tl => foo4(acc + hd, tl)
	}
}
