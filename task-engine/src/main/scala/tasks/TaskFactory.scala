package tasks

import scala.collection.Seq
import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
trait TaskFactory {
	def create(parents : mutable.Set[Task])(params : Any*) : Task
}
