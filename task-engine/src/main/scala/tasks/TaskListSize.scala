package tasks

import data._
import engine.Update

import scala.collection.mutable

/**
 * size(l : List) : Int = l match {
 * 	case Nil => 0
 * 	case x :: xs => 1 + size(xs)
 * }
 *
 * @author Mirko Köhler
 */
class TaskListSize(val list : IList[_]) extends Task[Int](list){


	override protected def internalTraverse(u : Update) : Iterable[Task[_]] = {

		list match {
			case IListEmpty() =>
				children.clear()
			case IListElement(head, tail) if children.count == 1 && children(0).checkTask(classOf[TaskListSize], tail) =>
				//do nothing
			case IListElement(head, tail) =>
				children.clear()
				spawn(u)(TaskListSizeFactory, tail)
			case _ => super.internalTraverse(u)
		}

		children
	}

	override protected def internalRecompute(u : Update) : Unit = {


		list match {
			case IListEmpty() =>
				result.update(0)
			case IListElement(head, tail) =>
				result.update(children(0).result.get.asInstanceOf[Int] + 1)
			case _ => super.internalRecompute(u)
		}


	}

	def canBeRecomputed : Boolean = list match {
		case IListEmpty() => true
		case IListElement(head, tail) => children.count == 1 //&& !children(0).isDirty
	}



}

object TaskListSizeFactory extends TaskFactory[Int] {
	override def create(params: Data*): Task[Int] = {
		if (params.size != 1)
			throw new IllegalArgumentException("List.size needs 1 parameter, got " + params)
		new TaskListSize(params(0).asInstanceOf[IList[_]])
	}
}


