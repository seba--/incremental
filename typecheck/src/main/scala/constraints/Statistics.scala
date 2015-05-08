package constraints

/**
 * Created by seba on 13/11/14.
 */

object Statistics {
  val mergeSolutionTime = "mergeSolutionTime"
  val constraintCount = "constraintCount"
  val constraintSolveTime = "constraintSolveTime"
  val finalizeTime = "finalizeTime"
  val typecheckTime = "typecheckTime"
  val mergeReqsTime = "mergeReqsTime"

  val keys = Seq(mergeSolutionTime, constraintCount, constraintSolveTime, finalizeTime, typecheckTime, mergeReqsTime)
}
