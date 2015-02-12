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

	/**
	 * True, if additional debug information should be printed.
	 */
	var debug = true

	/**
	 * Recomputes the value of the task if all values of child tasks are available. If the new value differs from the previous one the task will be flagged as dirty.
	 * @param u The class to which spawn updates are passed.
	 */
	def recompute(u : Update) : Unit = {
		if (debug) print(toTypeString + ".recompute -> " + result.get)
		internalRecompute(u)
		if (debug) println(" ==> " + result.get)
	}

	/**
	 * Traverses through a node and creates all child nodes that are currently available.
	 * @param u The class to which spawn updates are passed.
	 * @return All currently available child nodes.
	 */
	def traverse(u : Update) : Iterable[Task[_]] = {
		val r = internalTraverse(u)
		if (debug) println(toTypeString + ".traverse -> " + children)
		r
	}

	protected def internalRecompute(u : Update) : Unit = throw new UnsupportedOperationException("Don't know how to recompute task " + toString)
	protected def internalTraverse(u : Update) : Iterable[Task[_]] = throw new UnsupportedOperationException("Don't know how to traverse task " + toString)

	def canBeRecomputed : Boolean

	def checkTask(c : Class[_], p : Data*) : Boolean =
		c.isInstance(this) && p == params

	def hasDirtyParams : Boolean =
		params.foldRight(false)((data, b) => data.isDirty || b)

	def hasDirtyChildren : Boolean =
		_children.foldRight(false)((task, b) => task.isDirty || b)

	object children extends Iterable[Task[_]] {
		def apply(i : Int) : Task[_] = _children(i)

		def +=(t : Task[_]) = {
			_children += t
		}

		def count = _children.size

		def clear() {
			_children.clear()
		}

		override def iterator: Iterator[Task[_]] =
			_children.iterator
	}

	//Manages the result of the task. Whenever the result is changed, the task is marked dirty
	object result {
		//The result of the computation.
		private var _res : Result = null.asInstanceOf[Result]

		def get = _res//if (!isDirty) _res else throw new IllegalStateException("res is not available for task: " + toString)

		def apply() : Result = get

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
		toStringTree(0)
	}

	private def toStringTree(indent : Int) : String = {

		val tabs : String = "\t" * indent
		var s : String = tabs + toString

		children.foreach((t : Task[_]) => {
			s = s + "\n" + tabs + t.toStringTree(indent + 1)
		})

		s
	}

	def toTypeString : String =
		s"Task<${this.getClass.getSimpleName}>(${params.mkString(", ")})"

	override def toString: String =
		s"$toTypeString = ${if(!isDirty) result.get else "<Dirty> " + result.get}"

}


