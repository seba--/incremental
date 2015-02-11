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
class TaskListSize(l : IList[_]) extends Task[Int](l){

	override def internalRecompute(u : Update) : Unit = {

		println("recompute : " + toString)

		l match {
			case IListEmpty() =>
				res.update(0)
			case IListElement(head, tail) if children.count == 0 =>
				spawn(u)(TaskListSizeFactory, tail)
				res.update(children(0).result.asInstanceOf[Int] + 1)
			case IListElement(_, _) if children.count == 1 =>
				res.update(children(0).result.asInstanceOf[Int] + 1)
			case _ => super.internalRecompute(u)
		}

		println("recomputed : " + toString)

	}



}

object TaskListSizeFactory extends TaskFactory[Int] {
	override def create(params: Data*): Task[Int] = {
		if (params.size != 1)
			throw new IllegalArgumentException("List.size needs 1 parameter, got " + params)
		new TaskListSize(params(0).asInstanceOf[IList[_]])
	}
}


