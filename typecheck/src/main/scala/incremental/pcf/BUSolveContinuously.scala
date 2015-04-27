package incremental.pcf

import constraints.equality.config.{SolveContinuously, SolveContinuouslyCS}

/**
 * Created by seba on 13/11/14.
 */
class BUSolveContinuously extends BUChecker[SolveContinuouslyCS] {
  type CSFactory = SolveContinuously.type
  val csFactory = SolveContinuously
}

object BUSolveContinuouslyFactory extends BUCheckerFactory[SolveContinuouslyCS] {
  def makeChecker = new BUSolveContinuously
}