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


	override protected def internalTraverse() : Iterable[Task[_]] = {

		list match {
			case IListEmpty() =>
				children.clear()
				children
			case IListElement(head, tail) if children.count == 1 && children(0).checkTask(classOf[TaskListSize], tail) =>
				//do nothing
				children
			case IListElement(head, tail) =>
				children.clear()
				children += new TaskListSize(tail)
				children
			case _ => super.internalTraverse
		}


	}

	override protected def internalRecompute() : Unit = {


		list match {
			case IListEmpty() =>
				result <= 0
			case IListElement(head, tail) =>
				result <= children(0).result.get.asInstanceOf[Int] + 1
			case _ => super.internalRecompute
		}


	}

	def canBeRecomputed : Boolean = list match {
		case IListEmpty() => true
		case IListElement(head, tail) => children.count == 1 //&& !children(0).isDirty
	}



}

