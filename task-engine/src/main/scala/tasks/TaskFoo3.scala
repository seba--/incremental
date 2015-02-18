package tasks

import data._

/**
 *
 * @author Mirko Köhler
 */
class TaskFoo3(val acc : IValue[Int], val list : IList[Int]) extends Task[Int](acc, list){

	override protected def internalTraverse() : Iterable[Task[_]] = (acc, list) match {
		case (_, IListEmpty()) =>
			children.clear()
			children
		case (_, IListElement(head, tail)) if
			children.count == 1
				&& children(0).isInstanceOf[TaskFoo3]
				&& children(0).asInstanceOf[TaskFoo3].acc.v.get == head
				&& children(0).asInstanceOf[TaskFoo3].list == tail
		=>
			//do nothing
			children
		case (_, IListElement(head, tail)) =>
			children.clear()
			children += new TaskFoo3(IValue(head), tail)
			children
		case _ => super.internalTraverse()
	}

	override protected def internalRecompute() : Unit = list match {
		case IListEmpty() =>
			result <= acc.v
		case IListElement(head, tail) =>
			result <= acc.v.get + children(0).result.get.asInstanceOf[Int]
		case _ => super.internalRecompute()
	}


	def canBeRecomputed : Boolean = list match {
		case IListEmpty() => true
		case IListElement(head, tail) => children.count == 1 //&& !children(0).isDirty
	}
}

object TaskFoo3 {
	def foo3(acc: Int, l: List[Int]): Int =  l match {
		case Nil => acc
		case hd::tl => acc + foo3(hd, tl)
	}
}

