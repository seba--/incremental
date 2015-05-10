package constraints

/**
 * Created by seba on 13/11/14.
 */

object Statistics extends Enumeration {
  type Statistics = Value
  val mergeSolutionTime = Value
  val constraintCount = Value
  val constraintSolveTime = Value
  val finalizeTime = Value
  val typecheckTime = Value
  val mergeReqsTime = Value
}
