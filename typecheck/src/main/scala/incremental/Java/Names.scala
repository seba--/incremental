package incremental.Java

/**
 * Created by qwert on 04.04.15.
 */
trait Name {

}

case class PackageName(names: Seq[String]) extends Name
//case class PackageName(p: PackageName, id :String) extends Name

case class ExpressionName(names: Seq[String]) extends Name

case class MethodName(names: Seq[String]) extends Name