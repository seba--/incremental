package tasks

import scala.collection.Seq
import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
abstract class Task(val parents : mutable.Set[Task])(val params : Any*) {
	def this(p : Task)(pa : Any*) = this(mutable.Set(p))(pa : _*)

	private val _children : mutable.Buffer[Task] = mutable.Buffer()

	parents.foreach(p => p.children += this)
	initialize()

	def invalidate() : Unit = {
		valid.update(b = false)
	}

	def isValid = valid()
	def result = res()

	//Is called when the task is created.
	def initialize() : Unit
	//Is called when a child task is updated. TODO: should also be called if the data changes
	def update() : Unit

	object children {
		def apply(i : Int) : Task = _children(i)
		def +=(t : Task) = {
			_children += t
			valid.update(b = false)  // TODO: Should we update here?
		}

		def foreach(f : Task => Unit): Unit = {
			_children.foreach(f)
		}

		def count = _children.size
	}

	//Manages the result of the task. Whenever the result is changed, updates are pushed to the parents
	object res {
		//The result of the computation.
		private var _res : Any = null

		def apply() : Any = if (isValid) _res else throw new IllegalStateException("res is not available for task: " + toString())

		def update(e : Any) {
			if (e != _res) {
				_res = e
				valid.updateNoPush(b = true)
				pushUpdate()
			}
		}
	}


	private def pushUpdate(): Unit = {
		parents.foreach(_.update())
	}

	private object valid {
		private var _valid : Boolean = false

		def apply() : Boolean = _valid

		def updateNoPush(b : Boolean) =
			_valid = b


		def update(b : Boolean) {
			if (_valid != b) {
				_valid = b
				pushUpdate()
			}
		}
	}

	def spawn(factory : TaskFactory, params: Any*) : Task = {
		val t : Task = factory.create(mutable.Set(this))(params : _*)
	//	children += t
		t
	}

	def toStringTree : String = {
		var s = toString

		children.foreach(t => {
			s = s + "\n\t" + t.toStringTree
		})

		s
	}

	override def toString: String = {
		s"Task<${this.getClass.getSimpleName}>(${params.mkString(", ")}) = ${if(isValid) result else null}"
	}





}

object Task {


}


