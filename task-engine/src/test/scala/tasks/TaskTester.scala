package tasks

import data._
import engine.BottomUpUpdate

import org.junit.{Before, Test}
import org.junit.Assert._

/**
 * @author Mirko Köhler
 */
class TaskTester {

	private def test[T, DF](task : Task[T], f : DF => T, dataUpdate : Seq[() => Unit] , dataConvert : Seq[Data] => DF): Unit = {

		println("#### Init ##############################################")
		val c = BottomUpUpdate.update(task)
		val taskRes = task.result.get
		val fRes = f(dataConvert(task.params))

		assertEquals(fRes, taskRes)
		println(s"Result = $taskRes | # of recomputed nodes = $c")

		var i = 0
		dataUpdate.foreach( updateFun => {
			i = i + 1
			println("#### Update" + i + " ##########################################")
			updateFun()

			val c = BottomUpUpdate.update(task)
			val taskRes = task.result.get
			val fRes = f(dataConvert(task.params))

			assertEquals(fRes, taskRes)
			println(s"Result = $taskRes | # of recomputed nodes = $c")
		})
	}

	var l1 : IList[Int] = null
	var l1Update : Seq[() => Unit] = null

	var int1 : IValue[Int] = null
	var l1AndInt1Update : Seq[() => Unit] = null

	@Before
	def setUp(): Unit = {
		l1 = IList(1, 2, 3, 4)
		l1Update =
			List(
				() => l1.head <= 10,
				() => l1.tail.head <= 5,
				() => l1.tail.tail <= IList.empty,
				() => l1.head <= 3,
				() => {}
			)

		int1 = IValue(0)
		l1AndInt1Update =
			List(
				() => l1.head <= 10,
				() => int1.v <= 3,
				() => int1.v <= 8,
				() => l1.tail.head <= 5,
				() => l1.tail.tail <= IList.empty,
				() => int1.v <= 0,
				() => l1.head <= 3,
				() => int1.v <= 1,
				() => {}
			)
	}

	@Test
	def testFoo1(): Unit = {
		test(
			new TaskFoo1(l1),
			TaskFoo1.foo1,
		   	l1Update,
			(s : Seq[Data]) => s(0).asInstanceOf[IList[Int]].toList
		)
	}

	@Test
	def testFoo2(): Unit = {
		test(
			new TaskFoo2(l1),
			TaskFoo2.foo2,
			l1Update,
			(s : Seq[Data]) => s(0).asInstanceOf[IList[Int]].toList
		)
	}

	@Test
	def testFoo3(): Unit = {
		test(
			new TaskFoo3(int1, l1),
			(p : (Int, List[Int])) => TaskFoo3.foo3(p._1, p._2),
			l1AndInt1Update,
			(s : Seq[Data]) => (s(0).asInstanceOf[IValue[Int]].v.get ,s(1).asInstanceOf[IList[Int]].toList)
		)
	}

	@Test
	def testFoo4(): Unit = {
		test(
			new TaskFoo4(int1, l1),
			(p : (Int, List[Int])) => TaskFoo4.foo4(p._1, p._2),
			l1AndInt1Update,
			(s : Seq[Data]) => (s(0).asInstanceOf[IValue[Int]].v.get ,s(1).asInstanceOf[IList[Int]].toList)
		)
	}



}
