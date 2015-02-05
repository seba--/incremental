package tasks

import data.{IListEmpty, IList, IListElement}

import scala.collection.mutable

/**
 * size(l : List) : Int = l match {
 * 	case Nil => 0
 * 	case x :: xs => 1 + size(xs)
 * }
 *
 * @author Mirko Köhler
 */
class TaskListSize(p : mutable.Set[Task[_]])(l : IList[_]) extends Task[Int](p)(l){

	def update(): Unit =  {
		println("before update: " + this)
		l match {
			case IListEmpty() =>
				res.update(0)
			case IListElement(_, _) if children.count == 1 && !children(0).isDirty =>
				res.update(children(0).result.asInstanceOf[Int] + 1)
			case _ => _dirty = true
		}
		println("after update: " + this)
	}

	def initialize(): Unit = l match {
		case IListEmpty() => update()
		case IListElement(head, tail) => spawn(TaskListSizeFactory, tail)
	}

}

object TaskListSizeFactory extends TaskFactory[Int] {
	override def create(parents : mutable.Set[Task[_]])(params: Any*): Task[Int] = {
		if (params.size != 1)
			throw new IllegalArgumentException("List.size needs 1 parameter, got " + params)
		new TaskListSize(parents)(params(0).asInstanceOf[IList[_]])
	}
}


