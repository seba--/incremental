package engine

import data._
import tasks.{Task, TaskListSize}

import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
object Main {

	def main(args : Array[String]): Unit = {
		val l = new IListElement('a', new IListElement('b', new IListEmpty[Char]))
		val e0 = Exp(RegExpTerminal, mutable.Seq('a'), mutable.Seq())
		val e1 : Exp = Exp(RegExpAlt, mutable.Seq(), mutable.Seq(
			e0,
			Exp(RegExpTerminal, mutable.Seq('c'), mutable.Seq())
		))

		val t : Task[Int] = new TaskListSize(l)

		println("Update 1 ##########################################")
		BottomUpUpdate.update(null)(t)

		println("task tree:")
		println(t.toStringTree)
		print("> result = ")
		println(t.result)

		println("Update 2 ##########################################")
		l.tail <= new IListEmpty[Char]
		BottomUpUpdate.update(null)(t)
	//	t.children(0).update() //TODO: this is not how it is supposed to be!

		println("task tree:")
		println(t.toStringTree)
		println("> result = " + t.result)

	/*	l.head.update('b')
		BottomUpUpdate.update(null)(t)
	//	t.children(0).update() //TODO: this is not how it is supposed to be!

		println("task tree #########################################")
		println(t.toStringTree)
		println("> result = " + t.result)
            */
	}

}
