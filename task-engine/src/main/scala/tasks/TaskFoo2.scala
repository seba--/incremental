package tasks

import data._

/**
 *
 * @author Mirko Köhler
 */
class TaskFoo2(val list : IList[Int]) extends Task[Int](list){

	override protected def internalTraverse() : Iterable[Task[_]] = list match {
		case IListEmpty() =>
			children.clear()
			children
		case IListElement(head, tail) if children.count == 1 && children(0).checkTask(classOf[TaskFoo2], tail) =>
			//do nothing
			children
		case IListElement(head, tail) =>
			children.clear()
			children += new TaskFoo2(tail)
			children
		case _ => super.internalTraverse()
	}

	override protected def internalRecompute() : Unit = list match {
		case IListEmpty() =>
			result <= 0
		case IListElement(head, tail) if children(0).result.get.asInstanceOf[Int] > 5 =>
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

object TaskFoo2 {
	def foo2(l: List[Int]): Int =  l match {
		case Nil => 0
		case hd::tl =>
			val r = foo2(tl)
			if (r > 5)
				hd + r
			else
				hd - r
	}
}

