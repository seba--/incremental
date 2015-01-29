package engine

import data._
import tasks.{TaskInterpretRegExp, Task, TaskListSize}

import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
object Main {

	def main(args : Array[String]): Unit = {
		val l : IList = IListElement('a', IListElement('b', IListEmpty))
		val e0 = Exp(RegExpTerminal, mutable.Seq('a'), mutable.Seq())
		val e1 : Exp = Exp(RegExpAlt, mutable.Seq(), mutable.Seq(
			e0,
			Exp(RegExpTerminal, mutable.Seq('c'), mutable.Seq())
		))

		val t : Task = new TaskInterpretRegExp(mutable.Set.empty)(e1, l)
		//new TaskListSize(mutable.Set.empty)(l)

		//BottomUpUpdate.updateTask(t)

		println("task tree #########################################")
		println(t.toStringTree)
		println("> result = " + t.result)

		e0.values.update(0, 'b')
		t.children(0).update() //TODO: this is not how it is supposed to be!

		println("task tree #########################################")
		println(t.toStringTree)
		println("> result = " + t.result)

		l.updateHead('b')
		t.children(0).update() //TODO: this is not how it is supposed to be!

		println("task tree #########################################")
		println(t.toStringTree)
		println("> result = " + t.result)

	}

}
