package tasks

import scala.collection.Seq
import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
trait TaskFactory[Result] {
	def create(parents : mutable.Set[Task[_]])(params : Any*) : Task[Result]
}
