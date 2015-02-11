package tasks

import data.Data
import engine.Update

import scala.collection.Seq
import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
abstract class Task[Result](val params : Data*) extends Node {

	private val _children : mutable.Buffer[Task[_]] = mutable.Buffer()



	def result = res()

	def recompute(u : Update) : Unit = {
		internalRecompute(u)
	}

	def visit(u : Update) : Unit = {
		internalVisit(u)
	}

	protected def internalRecompute(u : Update) : Unit = throw new UnsupportedOperationException("Don't know how to recompute task " + toString)
	protected def internalVisit(u : Update) : Unit = throw new UnsupportedOperationException("Don't know how to visit task " + toString)

	def hasDirtyParams : Boolean =
		params.foldRight(false)((data, b) => data.isDirty || b)

	def hasDirtyChildren : Boolean =
		_children.foldRight(false)((task, b) => task.isDirty || b)

	object children {
		def apply(i : Int) : Task[_] = _children(i)
		def +=(t : Task[_]) = {
			_children += t
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

		def apply() : Any = _res//if (!isDirty) _res else throw new IllegalStateException("res is not available for task: " + toString)

		def update(e : Result) {
			if (e != _res) {
				_res = e
				_dirty = true
			}
		}
	}

	def spawn[T](u : Update)(factory : TaskFactory[T], params: Data*) : Task[T] = {
		val t : Task[T] = factory.create(params : _*)
		children += t
		u.notifySpawnTask(this, t)
		t
	}

	def toStringTree : String = {
		var s = toString

		children.foreach(t => {
			s = s + "\n\t" + t.toStringTree
		})

		s
	}

	override def toString: String =
		s"Task<${this.getClass.getSimpleName}>(${params.mkString(", ")}) = ${if(!isDirty) result else "<Dirty> " + result}"

}


