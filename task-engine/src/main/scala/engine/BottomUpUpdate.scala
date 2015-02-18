package engine

import tasks.Task

/**
 * @author Mirko Köhler
 */
object BottomUpUpdate extends Update {

	def update(t : Task[_]) : Int = {
		recomputeCount = 0
		update(null, t)
		recomputeCount
	}

	private var recomputeCount = 0

	private def update(parent : Task[_], t : Task[_]): Unit = {

		//Step 1: Update children first
		var children = t.traverse()
		children.foreach(update(t, _))

		//Step 2: Recompute when possible, else traverse
		var loop : Boolean = true
		while (loop) {

			if (children.isEmpty && t.hasDirtyParams) {
				t.recompute()
				recomputeCount = recomputeCount + 1
				t.params.foreach(_.visited())
				loop = false
			} else if (children.nonEmpty && (t.hasDirtyChildren || t.hasDirtyParams)) {
				if (t.canBeRecomputed) {
					t.recompute()
					recomputeCount = recomputeCount + 1
					t.params.foreach(_.visited())
					t.children.foreach(_.visited())
					loop = false
				} else {
					children = t.traverse()
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

}
