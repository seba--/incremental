package tasks

import data.Data

import scala.collection.Seq
import scala.collection.mutable

/**
 * @author Mirko Köhler
 */
trait TaskFactory[Result] {
	def create(params : Data*) : Task[Result]
}
