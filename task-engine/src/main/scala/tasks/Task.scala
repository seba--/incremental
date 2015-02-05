package tasks

import data.Data

import scala.collection.Seq
import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
abstract class Task[Result](val parents : mutable.Set[Task[_]])(val params : Data*) extends Node {

	private val _children : mutable.Buffer[Task[_]] = mutable.Buffer()

	parents.foreach(p => p.children += this)
	initialize()

	def result = res()

	//Is called when the task is created.
	def initialize() : Unit
	//Is called when a child task is updated. TODO: should also be called if the data changes
	def update() : Unit

	object children {
		def apply(i : Int) : Task[_] = _children(i)
		def +=(t : Task[_]) = {
			_children += t
			_dirty = true  // TODO: Should we update here?
		}

		def foreach(f : Task[_] => Unit): Unit = {
			_children.foreach(f)
		}

		def count = _children.size
	}

	//Manages the result of the task. Whenever the result is changed, updates are pushed to the parents
	protected object res {
		//The result of the computation.
		private var _res : Result = null.asInstanceOf[Result]

		def apply() : Any = if (!isDirty) _res else throw new IllegalStateException("res is not available for task: " + toString)

		def update(e : Result) {
			if (e != _res) {
				_res = e
				_dirty = true
			}
		}
	}

	def spawn[T](factory : TaskFactory[T], params: Any*) : Task[T] = {
		val t : Task[T] = factory.create(mutable.Set(this))(params : _*)
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
		s"Task<${this.getClass.getSimpleName}>(${params.mkString(", ")}) = ${if(!isDirty) result else null}"
	}
}


