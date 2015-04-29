package constraints

/**
 * Created by seba on 13/11/14.
 */

class Statistics {
  var preparationTime = 0.0
  var typecheckTime = 0.0
  var mergeSolutionTime = 0.0
  var constraintCount = 0
  var constraintSolveTime = 0.0
  var mergeReqsTime = 0.0
  var finalizeTime = 0.0

  def reset(): Unit = {
    preparationTime = 0.0
    typecheckTime = 0.0
    mergeSolutionTime = 0.0
    constraintCount = 0
    constraintSolveTime = 0.0
    mergeReqsTime = 0.0
    finalizeTime = 0.0
  }
}
