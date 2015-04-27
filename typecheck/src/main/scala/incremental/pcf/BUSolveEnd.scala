package incremental.pcf

import constraints.equality.config.{SolveEndCS, SolveEnd}

/**
 * Created by seba on 13/11/14.
 */
class BUSolveEnd extends BUChecker[SolveEndCS] {
  type CSFactory = SolveEnd.type
  val csFactory = SolveEnd
}

object BUSolveEndFactory extends BUCheckerFactory[SolveEndCS] {
  def makeChecker = new BUSolveEnd
}