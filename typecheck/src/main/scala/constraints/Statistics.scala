package constraints

/**
 * Created by seba on 13/11/14.
 */

object Statistics extends Enumeration {
  type Statistics = Value
  val constraintCount = Value
  val constraintSolveTime = Value
  val mergeSolutionTime = Value
  val mergeReqsTime = Value
  val finalizeTime = Value
  val typecheckTime = Value
}
