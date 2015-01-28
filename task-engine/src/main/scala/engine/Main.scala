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
		val e : Exp = Exp(RegExpAlt, mutable.Seq(), mutable.Seq(
			Exp(RegExpTerminal, mutable.Seq('a'), mutable.Seq()),
			Exp(RegExpTerminal, mutable.Seq('c'), mutable.Seq())
		))

		val t : Task = //new TaskInterpretRegExp(mutable.Set.empty)(e, l)
		new TaskListSize(mutable.Set.empty)(l)

		//BottomUpUpdate.updateTask(t)

		println("task tree #########################################")
		println(t.toStringTree)
		println("> result = " + t.result)
	}

}
