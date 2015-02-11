package engine

import tasks.Task

/**
 * @author Mirko Köhler
 */
trait Update {

	def notifySpawnTask(parent : Task[_], spawnedTask : Task[_]) : Unit

}
