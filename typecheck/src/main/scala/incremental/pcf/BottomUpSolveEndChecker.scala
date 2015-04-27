package incremental.pcf

import constraints.equality.config.{SolveEndCS, SolveEnd}

/**
 * Created by seba on 13/11/14.
 */
class BottomUpSolveEndChecker extends BUTypeChecker[SolveEndCS] {
  type CSFactory = SolveEnd.type
  val csFactory = SolveEnd
}

object BottomUpSolveEndCheckerFactory extends BUTypeCheckerFactory[SolveEndCS] {
  def makeChecker = new BottomUpSolveEndChecker
}