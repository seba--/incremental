package engine

import data._
import tasks.{Task, TaskListSize}
import tasks.Updateable

import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
object Main {

	def main(args : Array[String]): Unit = {
		val l = new IListElement('a', new IListElement('b', new IListEmpty[Char]))
		val e0 = new Exp(RegExpTerminal, mutable.Seq('a'), mutable.Seq())
		val e1 : Exp = new Exp(RegExpAlt, mutable.Seq(), mutable.Seq(
			e0,
			new Exp(RegExpTerminal, mutable.Seq('c'), mutable.Seq())
		))

		val t : Task[Int] = new TaskListSize(l)

		println("Update 1 ##########################################")
		BottomUpUpdate.update(t)

		println("task tree:")
		println(t.toStringTree)
		print("> result = ")
		println(t.result.get)

		println("Update 2 ##########################################")
		l.tail.get.asInstanceOf[IListElement[Char]].head := 'c'
		BottomUpUpdate.update(t)

		println("task tree #########################################")
		println(t.toStringTree)
		println("> result = " + t.result.get)

		println("Update 3 ##########################################")
		l.tail := IListEmpty[Char]()
		BottomUpUpdate.update(t)

		println("task tree:")
		println(t.toStringTree)
		println("> result = " + t.result.get)



	}


  def foo1(l: List[Int]): Int =  l match {
    case Nil => 0
    case hd::tl if hd > 5 => hd + foo1(tl)
    case hd::tl => hd - foo1(tl)
  }

  def foo2(l: List[Int]): Int =  l match {
    case Nil => 0
    case hd::tl =>
      val r = foo2(tl)
      if (r > 5)
        hd + r
      else
        hd - r
  }

  def foo3(l: List[Int]) = foo3(0, l)
  def foo3(acc: Int, l: List[Int]): Int =  l match {
    case Nil => acc
    case hd::tl => acc + foo3(hd, tl)
  }

  def foo4(l: List[Int]) = foo3(0, l)
  def foo4(acc: Int, l: List[Int]): Int =  l match {
    case Nil => acc
    case hd::tl => foo4(acc + hd, tl)
  }



}
