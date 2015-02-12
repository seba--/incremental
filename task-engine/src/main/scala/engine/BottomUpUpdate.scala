package engine

import tasks.Task

/**
 * @author Mirko Köhler
 */
object BottomUpUpdate extends Update {

	def update(t : Task[_]) : Unit =
		update(null, t)

	def update(parent : Task[_], t : Task[_]): Unit = {

		//Step 1: Update children first
		var children = t.traverse(this)
		children.foreach(update(t, _))

		//Step 2: Recompute when possible, else traverse
		var loop : Boolean = true
		while (loop) {

			if (children.isEmpty && t.hasDirtyParams) {
				t.recompute(this)
				t.params.foreach(_.visited())
				loop = false
			} else if (children.nonEmpty && (t.hasDirtyChildren || t.hasDirtyParams)) {
				if (t.canBeRecomputed) {
					t.recompute(this)
					t.params.foreach(_.visited())
					t.children.foreach(_.visited())
					loop = false
				} else {
					val n = t.traverse(this)
					n.drop(children.size).foreach(update(t, _))
					children = n
				}
			} else {
				loop = false
			}
		}

		/*println("update : " + t)

		if (t.children.count == 0 && t.isDirty) {
			t.recompute(this)
		} else if (t.children.count == 0 && t.hasDirtyParams) {
			t.recompute(this)
			t.params foreach (_.visited())
		} else if (t.children.count  > 0) {
			t.children foreach (update(t)(_))
			if (t.hasDirtyChildren || t.hasDirtyParams) {
				t.recompute(this)
				t.params foreach (_.visited())
				t.children foreach (_.visited())
			}
		}

		if (parent == null)
			t.visited()

        */

	}

	def notifySpawnTask(parent : Task[_], spawnedTask : Task[_]) : Unit = {}
	//	update(parent)(spawnedTask)

}
