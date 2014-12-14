package incremental.systemf
import incremental.ConstraintOps._
import incremental.Exp.Exp
import incremental.Exp._
import incremental.Type.Companion._
import incremental._

/**
 * Created by seba on 14/11/14.
 */
class DownUpChecker extends pcf.DownUpChecker {

 //TODO implement for systemf

}

object DownUpCheckerFactory extends TypeCheckerFactory[Type] {
  def makeChecker = new DownUpChecker
}