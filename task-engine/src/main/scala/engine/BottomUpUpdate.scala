package engine

import tasks.Task

/**
 * @author Mirko Köhler
 */
object BottomUpUpdate extends Update {

	def update(parent : Task[_])(t : Task[_]): Unit = {
		println("update : " + t)

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



	}

	def notifySpawnTask(parent : Task[_], spawnedTask : Task[_]) : Unit =
		update(parent)(spawnedTask)

}
