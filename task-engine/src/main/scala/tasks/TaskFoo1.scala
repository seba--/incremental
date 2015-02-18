package tasks

import data._

/**
 *
 * @author Mirko Köhler
 */
class TaskFoo1(val list : IList[Int]) extends Task[Int](list){

	override protected def internalTraverse() : Iterable[Task[_]] = list match {
		case IListEmpty() =>
			children.clear()
			children
		case IListElement(head, tail) if children.count == 1 && children(0).checkTask(classOf[TaskFoo1], tail) =>
			//do nothing
			children
		case IListElement(head, tail) =>
			children.clear()
			children += new TaskFoo1(tail)
			children
		case _ => super.internalTraverse()
	}




	override protected def internalRecompute() : Unit = list match {
		case IListEmpty() =>
			result <= 0
		case IListElement(head, tail) if head > 5 =>
			result <= head + children(0).result.get.asInstanceOf[Int]
		case IListElement(head, tail) =>
			result <= head - children(0).result.get.asInstanceOf[Int]
		case _ => super.internalRecompute()
	}




	def canBeRecomputed : Boolean = list match {
		case IListEmpty() => true
		case IListElement(head, tail) => children.count == 1 //&& !children(0).isDirty
	}
}

object TaskFoo1 {
	def foo1(l: List[Int]): Int =  l match {
		case Nil => 0
		case hd :: tl if hd > 5 => hd + foo1(tl)
		case hd :: tl => hd - foo1(tl)
	}
}

